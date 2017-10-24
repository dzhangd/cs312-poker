:- module(cards, [card/2]).

% card(V, S) is true if V is a valid card value and S is a valid suit
card(V, S) :-
    number(V),
    between(2, 10, V),
    valid_suit(S).
card(V, S) :-
 member(V, [ace, jack, queen, king]),
 valid_suit(S).

valid_suit(S) :-
    member(S, [spades, hearts, clubs, diamonds]). 
