library(stringr)
library(tidyverse)
library(glue)
library(retry)


# TO DO:
# - keep win/loss stats


# command line blackjack game
# uses 1 deck with 4 suits which reshuffle when ~5 cards are left
# you start with two cards and add them up, if you have 21, then you have a blackjack
# dealer is the computer
# player plays first, then dealer goes
# the better score not exceeding 21 wins
# if the dealer and player are equal, then the hand is a tie
# any score over 21 is a bust
# player starts off with 100 chips to bet
# can bet any amount they want at start, but must bet at least 1
# if player looses: the bet is taken by the dealer
# if player wins: the player wins the amount of the orginal bet
# if player wins by with a natural blackjack: the player wins 1.5x the orginal bet
# if game ends with tie: player doesn't loose or win any chips

shufflecards <- function() {
  # pick a card from the deck
  # a suit of cards in blackjack assumes the values 2 - 10,
  # with face cards as 10 and ace as 11 for start
  # 4 suits in deck, so sequence repeats
  
  cards <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
  suits <- c("Hearts", "Spades", "Clubs", "Diamonds")
  deck <- purrr::map(suits, ~stringr::str_c(cards, .x, sep = " of ")) %>% unlist() # 4 suits
  suffled_deck <- sample(deck) # shuffles the cards
  return(suffled_deck)
  
}


# counting cards in the hand ----------------
int_hand <- function(hand) {
  # turn the full name of card into its numerical value
  hand <- case_when(str_detect(hand, "^A") ~ "11",
                    str_detect(hand, "^K|^Q|^J") ~ "10",
                    .default = as.character(hand))
  
  hand_int <- str_extract(hand, "[[:digit:]]+") %>% as.integer()
  
  return(hand_int)
}


total_hand <- function(hand) { 
  # need to know many aces are in the hand
  # change face value cards to integer values
  # the ace can be 11 or 1
  # figure out which one it should be based on sum of hand
  
  aces <- str_count(hand, "^A") %>% sum()
  hand <- int_hand(hand) 
  tot <- sum(hand)
  
  while( aces > 0 && tot > 21 ) {
    # this will switch the Ace from 11 to 1 
    tot <- tot - 10
    aces <- aces - 1
  }
  
  return(tot)
}


print_total <- function(hand) {
  # checks the number of aces and lets you know if you have +/- 10 available in the hand
  aces  <- str_count(hand, "^A") %>% sum()
  tot <- total_hand(hand) # total of hand
  tot_a <- sum(int_hand(hand)) # total of hand with all aces as 11
  hand_str <- str_c(hand, collapse = ", ")
  
  
  while(aces > 0 && tot_a >= tot) {
    tot_a <- tot_a - 10
    aces <- aces - 1
  }
  
  if(aces == 0) {
    print(glue("The player has these cards {hand_str} for a total of {tot}!")) 
  } 
  else if(aces > 0 && tot <= 20 && tot_a == tot) {
    tot_a <- tot_a - 10
    print(glue("The player has these cards {hand_str} for a total of {tot} or {tot_a}!")) 
  }
  else if(tot == 21) {
    print(glue("The player has these cards {hand_str} for a total of {tot}!")) 
    r <- testblackjack(hand)
  }
}



# get input from user ---------------------

bet_input <- function(chips) {
  # player inputs bet amount
  # check that the bet is an integer greater of equal to 1
  
  bet <- as.integer(readline(prompt = "Place your bet: ")) # TODO - make this a test for int only
  
  if(bet < 1) {
    print("Oops! Bet must be an integer greater or equal to 1.")
    stop("Oops! Bet must be an integer greater or equal to 1.")
  }
  if(bet > chips) {
    print(glue("Oops! You bet more chips than you have. You have {chips} chips to bet."))
    stop(glue("Oops! You bet more chips than you have. You have {chips} chips to bet."))
  }
  # if(bet >= 1 && bet <= chips ) break   #TODO add this # print "Oops! Bet must be an integer greater or equal to 1.")
 
  return(bet) 
}



hit_or_stand <- function(){
  hs <- readline(prompt = "Hit or Stand (h or s): ") %>% str_to_lower()
  
  if( "h" != hs && "s" != hs) {
    print("Please type h or s to Hit or Stand")
    stop("error")
    }
   
  return(hs)
}

# rules of black jack --------------------        
test_blackjack <- function(hand) {
  total <- total_hand(hand)
  if_else(length(hand) == 2 && total == 21, TRUE, FALSE)
}

who_won <- function(dbust, pbust, totp, totd) {
	# +1 if player wins
	# -1 if dealer wins
	# 0 if push
  result <- case_when(pbust ~ -1,
                      dbust ~ 1,
                      totp < totd ~ -1,
                      totp > totd ~ 1,
                      totd == totp ~ 0)
  
  
	if(pbust) {
	  print("The player busts. The dealer wins!")
	}
	else if(dbust) {
		print("The dealer busts. The player wins!")
	}
	else{
		if(totd > totp) print("The dealer wins!") 
		if(totp > totd) print("The player wins!")
		if(totd == totp) print("It's a draw!") 
	}
  
	return(result)
}


blackjack <- function() {
  print("Welcome to Blackjack!")
  pchips <- 100 # player starting amount of chips 
  deck <- shufflecards() # shuffle the deck at the start of every new game
  cardnum <- 1 # initialize start of deck
  wins <- 0 # total player wins
  games <- 0 # total number of hands played
  
  
  while(TRUE){
    # begins every new hand
    
    pbust <- FALSE  # player busted flag
    dbust <- FALSE  # computer busted flag
    pwin <- FALSE # player win flag
    dwin <- FALSE # dealer win flag 
    
    
    # player bets to play
    print(glue("You have {pchips} chips to bet")) # tell the player the bet from the player
    bet <- retry(bet_input(pchips), when = "Oops") # get the bet from the player
    
    
    # set up the hands for player and dealer
    pcards <- c()
    dcards <- c()
    if(cardnum >= 47) { # reshuffle when ~5 cards are left
      print("Shuffling the deck")
      deck <- shufflecards() # shuffle the deck
      cardnum <- 1 # re-intialize start of deck
      
    }
    
    pcards <- c(pcards, deck[cardnum])
    dcards <- c(dcards, deck[cardnum+1])
    pcards <- c(pcards, deck[cardnum+2])
    dcards <- c(dcards, deck[cardnum+3])
    cardnum <- cardnum + 4 # account for the 4 cards used in deal
    
    print(glue("The dealer shows a {dcards[1]}.") )
    
    # player goes first
    # PLAYERS TURN
    while(TRUE) {
      
      totp <- total_hand(pcards)
      print_total(pcards)
      
      if(totp > 21) {
        pbust <- TRUE
        break
      }
      
      else if(totp < 21) {
        # get hit or stand input
        hs <- retry(hit_or_stand(), when = "error")
        
        if(hs == "h") {
          pcards <- c(pcards, deck[cardnum + 1])
          cardnum <- cardnum + 1 # increase to take the next card
        } else if(hs == "s") {
          pcards <- pcards
          break
        }
      }
      else if(totp == 21) {
        print_statement <- if_else(test_blackjack(pcards), "Blackjack!!!", "")
        print(print_statement)
        break
      }
    }  
    
    
    # dealer's turn
    totd <- total_hand(dcards) 
    if(! pbust) { # dealer only goes if player didn't bust 
      while(TRUE) {
        totd <- total_hand(dcards) 
        while(totd <= 16) { # dealer hits until his hand value is 17 or greater
          dcards <- c(dcards, deck[cardnum + 1])
          totd <- total_hand(dcards)
          cardnum <- cardnum + 1
        }
        
        if(totd > 21) {
          dbust <- TRUE
          break
        }
        else if(totd >= 17 && totd < 21) {
          print(glue("The dealer has a total of {totd}."))
          break
        }
        else if(totd == 21) {
          print_statement <- if_else(test_blackjack(dcards), "Dealer has blackjack!", "")
          print(glue("The dealer has a total of {totd}. {print_statement}"))
          break
        }
        
      }
    }
    
    # 
    #     
    #     
    # Who won?
    result <- who_won(dbust, pbust, totp, totd)
    wins <- if_else(result == 1, wins + 1, wins)
    games <- games + 1
    
    
    # Award the bet
    if(result == 1 && test_blackjack(pcards) ) {
      # award more points for natural blackjack
      pchips <- pchips + 1.5*bet
      # print(glue("You win {pchips}!"))
    } else {
      pchips <- pchips + result*bet
    }
    
    
    # Do you want to play again?
    if(pchips > 0) {
      print(glue("Player has {pchips} chips left! Do you want to continue?"))
      exit <- readline("Press Enter to Continue (q to quit): ")
    } else {
      print("Player doesn't have any chips left!")
      exit <- "q"
    }
    
    
    if(exit == "q") {
      print(glue("Player won {wins} games out of {games} for a win rate of {round(100 * (wins/games), 2)}%."))
      
      earned <- pchips - 100
      
      if(earned > 0) {
        print(glue("Player ended with {pchips} chips. That's {earned} more than player began with! Congrats! Thanks for playing!"))
      } else{
        print(glue("Player ended with {pchips} chips. Thanks for playing!"))
      }
      
      break
    }
    
  }
}

