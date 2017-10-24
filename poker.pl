:- use_module(cards).

%% Modified Poker-style game

% ! Poker (H, K) where H is a list of 5 cards and K is a list of =< 5
% cards to keep from H in order to maximize your win
% TODO: Actually make it
poker(H, K):-
    valid_hand(H),
    keep(H, K).

% valid_hand(H) is true when H is a valid hand of cards
valid_hand(H):-
    length(H, 5),
    no_duplicate_cards(H).

% valid_hand([card(ace, spades), card(2, hearts), card(7, clubs), card(2, diamonds), card(king, clubs)])

% no_duplicate_cards(H) is true when H only contains unique valid cards
no_duplicate_cards([]).
no_duplicate_cards([card(V, S)|T]) :-
	\+ member(card(V, S), T),
	no_duplicate_cards(T).
    
% keep(H, K) finds the best cards to keep K from hand H
% TODO: how do we do this

keep(H, K):-.



%!Rules
% TODO: be able to parse a list into each type instead of a dumb string

value("royal straight flush" , 250).
value("five of a kind", 60).
value("straight flush", 25).
value("four of a kind", 20).
value("Full House", 10).
value("Flush", 4).
value("Straight", 3).
value("Three of a Kind", 1).
value("Two Pairs", 1).
value("No Combo", 0).
