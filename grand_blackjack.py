#!/usr/bin/env python

# command line blackjack game
# uses 1 deck with 4 suits which reshuffle when ~5 cards are left
# you start with two cards and add them up, if you have 21, then you have a blackjack
# dealer is the computer
# player plays first, then dealer goes
# the better score not exceeding 21 wins
# if the dealer and player are equal, then the hand is a tie
# any score over 21 is a bust
# player starts off with 100 chips to bet
# can bet any amount they want at start, but must best at least 1
# if player looses: the bet is taken by the dealer
# if player wins: the player wins the amount of the orginal bet
# if player wins by with a natural blackjack: the player wins 1.5x the orginal bet
# if game ends with tie: player doesn't loose or win any chips

import random

def shufflecards():
    # pick a card from the deck
    # a suit of cards in blackjack assumes the values 2 - 10,
    # with face cards as 10 and ace as 11 for start
    # 4 suits in deck, so sequence repeats
    cards = ['2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A']
    deck = cards # 1 suit
    deck.extend(deck) # 2 suits
    deck.extend(deck) # 4 suits
    # shuffle the cards
    random.shuffle(deck)
    return deck

def getbet(chips):
    # player inputs bet amount
    # check that the bet is an integer greater of equal to 1
    while True:
        try:
            bet = int(raw_input("Place your bet: "))
            if bet < 1:
                print "Oops! Bet must be an integer greater or equal to 1."
                continue
            if bet > chips:
                print "You bet more chips than you have. You have %d chips to bet." % (chips)
                continue
            break
        except ValueError:
            print "Oops! Bet must be an integer greater or equal to 1."
    return bet


def inthand(hand):
	hand = [card.replace('A', '11') for card in hand]
	hand = [card.replace('Q', '10') for card in hand]
	hand = [card.replace('J', '10') for card in hand]
	hand = [card.replace('K', '10') for card in hand]
	hand = [int(card) for card in hand]
	return hand

def total(hand):
# need to know many aces are in the hand
	aces = hand.count('A')
	hand = inthand(hand) # change face value cards to integer values
    # the ace can be 11 or 1
    # figure out which one it should be based on sum of hand
	tot = sum(hand)
	while aces > 0 and tot > 21:
    	# this will switch the ace from 11 to 1
		tot = tot - 10
		aces -= 1
	return tot

def testblackjack(hand):
    if len(hand) == 2:
        print "Blackjack!!!"
    return 
    
def printtotal(hand):
    # checks the number of aces and lets you know if you have +/- 10 avilable in the hand
    aces = hand.count(11)
    tot = total(hand) # total of hand
    tot_a = sum(inthand(hand)) # total of hand with all aces as 11
    while aces > 0 and tot_a > tot:
        tot_a = tot_a - 10
        aces -= 1
    if aces == 0:
        print "The player has these cards %s for a total of %d!" % (hand,tot)
    elif aces > 0 and tot <= 20 and tot_a == tot:
        tot_a = tot_a - 10
        print "The player has these cards %s for a total of %d or %d!" % (hand,tot,tot_a)
    elif tot == 21:
        print "The player has these cards %s for a total of %d!" % (hand,tot)
        r = testblackjack(hand)
    return
            
def gethitorstand():
    while True:
        hs = raw_input("Hit or Stand (h or s): ").lower()
        if 'h' in hs or 's' in hs:
            break
        else:
            print "Please type h or s to Hit or Stand"
    return hs
                        
def whowon(dbust, pbust, totp, totd):
    # +1 is player wind
    # -1 is dealer wins
    # 0 is push
    if pbust:
        print "The player busts. The dealer wins!"
        result = -1
    elif dbust:
        print "The dealer busts. The player wins!"
        result = 1
    else:
        if totd > totp:
            print "The dealer wins!"
            result = -1
        if totp > totd:
            print "The player wins!"
            result = 1
        if totd == totp:
            print "It's a draw!"
            result = 0
    return result
    
while True:
    # begins the game
    print "Welcome to Blackjack!"
    pchips = 100 # player starting amount of chips
    # set up the deck
    deck = shufflecards() # shuffle the deck at the start of every new game
    cardnum = 0 # intialize start of deck
    while True:
        # begins every new match
        pbust = False  # player busted flag
        dbust = False  # computer busted flag
        pwin = False # player win flag
        dwin = False # dealer win flag
        # Player bets to play
        print  "You have %d chips to bet" % (pchips)
        # get the bet from the player
        bet = getbet(pchips)
        # set up the hand
        pcards=[]
        dcards = []
        if cardnum >= 47: # reshuffle when ~5 cards are left
            print "Shuffling the deck"
            deck = shufflecards() # shuffle the deck
            cardnum = 0 # re-intialize start of deck
        pcards.append(deck[cardnum])
        dcards.append(deck[cardnum+1])
        pcards.append(deck[cardnum+2])
        dcards.append(deck[cardnum+3])
        cardnum=cardnum+4
        print "The dealer shows a %s." % (dcards[0])
        # player goes first
        while True:
            totp = total(pcards)
            printtotal(pcards)
            if totp > 21:
                pbust = True
                break
            elif totp < 21:
                # get hit or stand
                hs = gethitorstand()                    
                if 'h' in hs:
                    pcards.append(deck[cardnum])
                    cardnum += 1 # increase to take the next card
                elif 's' in hs:
                    break
            if totp == 21:
                break
        # dealer's turn
        totd=0
        if not pbust: # dealer only goes if player didn't bust
            while True:
                # dealer hits until his hand value is 17 or greater            
                totd = total(dcards)
                while totd <= 16:
                    dcards.append(deck[cardnum])
                    totd = total(dcards)
                    cardnum += 1
                if totd > 21:
                    print "The dealer has these cards %s for a total of %d!" % (dcards,totd)
                    dbust = True
                    break
                if totd >= 17 and totd < 21:
                    print "The dealer has these cards %s for a total of %d!" % (dcards,totd)
                    break
                if totd == 21:
                    print "The dealer has these cards %s for a total of %d!" % (dcards,totd)
                    if len(dcards)==2:
                        print "Dealer has blackjack!"
                    break

        # Who won?
        result = whowon(dbust, pbust, totp, totd)
        # Award the bet
        if result == 1 and len(pcards)==2 and totp == 21:
            # award more points for natural BlackJack
            pchips = pchips + 1.5*bet
        else:
            pchips = pchips + result*bet
        # Play again?
        if pchips > 0:
            print "Player has %d chips left! Do you want to continue?" % (pchips)
            exit = raw_input("Press Enter to Continue (q to quit): ").lower()
        else:
            print "Player doesn't have any chips left!"
            exit = "q"
        if 'q' in exit:
            break
    break
totbet = pchips - 100
if totbet > 0:
    print "Player ended with %d chips. That's %d more than player began with! Congrats! Thanks for playing!" % (pchips,totbet)
else:
    print "Player ended with %d chips. Thanks for playing!" % (pchips)      
