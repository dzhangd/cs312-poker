%% Modified Poker-style game

% ! Poker (H, K) where H is a list of 5 cards and K is a list of =< 5
% cards to keep from H in order to maximize your win
% TODO: Actually make it
poker(H, K):-
    valid_hand(H),
    keep(H, K).

% valid_hand(H) is only true if there is a valid hand
valid_hand([card(V, S)|T]):-
    \+ duplicates([H|T]).

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
