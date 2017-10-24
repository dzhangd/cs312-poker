%% Modified Poker-style game

% ! Poker (H, K) where H is a list of 5 cards and K is a list of =< 5
% cards to keep from H in order to maximize your win
% TODO: Actually make it
poker(H, K):-
    valid_hand(C),
    find_best_combo(H, C),
    common(H, C, K).

% valid_hand(H) is only true if there is a valid hand
valid_hand([H|T]):-
    \+ duplicates([H|T]),
    valid_card(T).

% common(H, C, K) returns the common cards K between H and C

% find_best_combo(H, C) finds the best combo C from hand H
% TODO: actually do it


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


