Blackjack
=========

Simple command line blackjack program for python and R for the computer and a player.
 
Uses 1 deck with 4 suits and reshuffle when about 5 cards are left. The player starts with two cards and add them up, if you have 21, then you have a blackjack! If you have over 21, you bust and loose. Otherwise, the better score between the dealer and the player wins.The player plays first, then the dealer takes its turn.

The player starts off with 100 chips to bet and can bet any amount they want at start, but must bet at least 1.

- if player looses: the bet is taken by the dealer
- if player wins: the player wins the amount of the original bet
- if player wins by with a natural blackjack: the player wins 1.5x the orginal bet
- if game ends with tie: player doesn't loose or win any chips


Python verion: `python grand_blackjack.py`
R version: `blackjack()`
