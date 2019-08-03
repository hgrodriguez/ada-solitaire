ada-solitaire
=============

An Ada implementation for one solitaire game

## Background
I am an OO programmer knowing Java,  Python and C++.
Ada I was always interested in, but never had the chance to get on with it.

## For the Ada code/implementation
This project here is my first Ada implementation using my OO background.

It may not be perfect Ada-ese, but I tried to combine my knowledge with the Ada language.

I am a big fan of:  
1. naming procedures/functions/methods in a way, that they are self explanatory to minimize the documentation effort and risk of deviation of comments and code.
2. having unit tests as many as possible to make sure, that the code is doing what it is supposed to

I used the switches for code style guidelines to enforce the same layout of code everywhere.

## Game description
Goal of the game:  
Fill the foundation in ascending ranking order up to all four kings.

This version of Solitaire consists of three elements:
* Stock (where the pile of cards is and cards are fetched from throughout the game)
* Fondation (where the cards possible are moved into the final place)
* Tableau (where the cards can be moved around to achieve to fill the Foundation)

The general layout at the beginning of the game is (the dots shall be spaces...):

`F F F F.....S`

`C C C C C C C`  
`..C C C C C C`  
`....C C C C C`  
`......C C C C`  
`........C C C`  
`..........C C`  
`............C`  


Rules:
1. Foundation  
   The foundation has four stacks for each suit.  
   The foundation accepts only cards which have one rank higher than present on the corresponding suit stack.  
   If empty, the first card acceptable is an Ace.  
   A stack is closed, when the King is put on top.
1. Stock:  
   The stock provides cards into the tableau, until empty.  
   You can pull cards from the stock any time.  
   As long as there are more than seven cards in the stock, seven cards will be put into the tableau.
   If there are less than seven cards left, then all those cards will be put into the tableau.
   Pulled cards land as the bottom card at every stack, beginning from the left, on the tableau.  
   As long as there are enough cards in the stock, the stock will fill seven (= amount of stacks in the tableau) into the tableau.  
   If less than seven cards are left on the stock, then the remaining cards will fill the tableau stacks from the left.
1. Tableau:  
   1. You **have to** move a card onto the foundation, if it is the bottom card on any pile of cards in the tableau and can be accepted by the foundation.  
      E.g., an Ace **never** stays as a bottom card in any stack.
   1. If a stack is empty, the only card this stack can accept is a king of any suit.
   1. You can move any bottom card from a souce stack, given that:  
      * There is a destination stack with a bottom card having the opposite color (red <-> black) and is exactly one rank higher than the card to be moved.
   1. You can move any bottom pile of cards from a source stack, given that:  
      * There is a destination stack with a bottom card having the opposite color (red <-> black) and is exactly one rank higher than the top card of the pile of cards to be moved.

## Roadmap (without dates, as this is a pet project and I can only work so much on it)

V0.1: get all the basic parts into shape (including the rule checking) to be able to play with it

V1.0: have an ASCII command line interface available for interactive playing

V2.0: move the _obvious_ cards from the tableau onto the foundation automatically. Provide hints for next cards to be available for playing.

V3.0: have a GUI interface available for interactive playing

V4.0: implement strategies, that the computer plays the game.

V-FUTURE: port it to an embedded system for playing with a small touch display
