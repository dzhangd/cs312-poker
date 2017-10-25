:- module(cards, [card/2]).

% card(V, S) is true if V is a valid card value and S is a valid suit
card(V, S) :-
 member(V, [ace, jack, queen, king, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
 valid_suit(S).

% valid_suit(S) is true if S is one of the four standard suits
valid_suit(S) :-
    member(S, [spades, hearts, clubs, diamonds]).
