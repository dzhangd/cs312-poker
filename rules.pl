:- module(rules, [value/2]).
:- use_module(cards).

%%%%%%%%%%%%%%%

% value(H, V) is true when V is a possible value from hand H

% Rules
% Assume sorted hand

% Royal Straight Flush : 250pt
value(H, 250):-
    permutation( H, [card(10,X),card(jack,X),card(queen,X),card(king,X),card(ace,X)]).

% Five of a Kind: 60pt
% TODO: only if we decide to use jokers

% Straight Flush: 25pt
value(H, 25):-
    value(H, 4),
    value(H, 3).

% Four of a Kind : 20pt
value(H, 20) :-
    permutation(H, [_,card(A,_),card(A,_),card(A,_),card(A,_)]).


% Full House : 10 pt
value( H, 10) :-
    permutation(H, [card(A,_),card(A,_),card(A,_),card(B,_),card(B,_)]).

% Flush : 4pt

value(H, 4) :-
    permutation(H, [card(_,X),card(_,X),card(_,X),card(_,X),card(_,X)]).

% Straight : 3pt
% TODO: Is there a better way to find a circular sequence
value(H, 3):-
    permutation(H, [card(A,_),card(B,_),card(C,_),card(D,_),card(E,_)]),
    append(_,
           [card(A,_),card(B,_),card(C,_),card(D,_),card(E,_)|_],
           [card(2,_),card(3,_),card(4,_),card(5,_),card(6,_),card(7,_),card(8,_),card(9,_),card(10,_),card(jack,_),card(queen,_),card(king,_),card(ace,_)]).

% Three of a Kind : 1pt
value(H, 1):-
    permutation(H, [_,card(A,_),card(A,_),card(A,_),_]).

% Two Pairs : 1pt
value(H, 1):-
    permutation(H, [_,card(A,_),card(A,_),card(B,_),card(B,_)]).








