%% Modified Poker-style game

% ! Poker (H, K) where H is a list of 5 cards and K is a list of =< 5
% cards to keep from H in order to maximize your win
% TODO: Actually make it
poker(H, K):-.



%!Rules
% TODO: be able to parse a list into each type

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


