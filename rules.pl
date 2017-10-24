:- module(rules, [value/2]).

% Rules
% Assume sorted hand

% Royal Straight Flush : 250pt
value([card(10,X),card(jack,X),card(queen,X),card(king,X),card(ace,X)], 250).

% Five of a Kind: 60pt
% TODO: only if we decide to use jokers

% Straight Flush: 25pt
value(H, 25):-
    value(H, 4),
    value(H, 3).

% Four of a Kind : 20pt
value([_,card(A,_),card(A,_),card(A,_),card(A,_)], 20).
value([card(A,_),card(A,_),card(A,_),card(A,_),_], 20).

% Full House : 10 pt
value([card(A,_),card(A,_),card(A,_),card(B,_),card(B,_)], 10).
value([card(A,_),card(A,_),card(B,_),card(B,_),card(B,_)], 10).

% Flush : 4pt
value([card(_,X),card(_,X),card(_,X),card(_,X),card(_,X)], 4).

% Straight : 3pt
% TODO: Is there a better way to find a circular sequence
value([card(2,_),_,_,_,card(6,_)], 3).
value([card(3,_),_,_,_,card(7,_)], 3).
value([card(4,_),_,_,_,card(8,_)], 3).
value([card(5,_),_,_,_,card(9,_)], 3).
value([card(6,_),_,_,_,card(10,_)], 3).
value([card(7,_),_,_,_,card(jack,_)], 3).
value([card(8,_),_,_,_,card(queen,_)], 3).
value([card(9,_),_,_,_,card(king,_)], 3).
value([card(10,_),_,_,_,card(ace,_)], 3).
value([card(2,_),card(jack,_),_,_,card(ace,_)], 3).
value([card(2,_),card(3,_),card(queen,_),_,card(ace,_)], 3).
value([card(2,_),card(3,_),card(4,_),card(king,_),card(ace,_)], 3).


% Three of a Kind : 1pt
value([_,card(A,_),card(A,_),card(A,_),_], 1).
value([_,_,card(A,_),card(A,_),card(A,_)], 1).
value([card(A,_),card(A,_),card(A,_),_,_], 1).

% Two Pairs : 1pt
value([card(A,_),card(A,_),card(B,_),card(B,_),_], 1).
value([_,card(A,_),card(A,_),card(B,_),card(B,_)], 1).
value([card(A,_),card(A,_),_,card(B,_),card(B,_)], 1).
