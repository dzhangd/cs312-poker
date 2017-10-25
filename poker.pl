:- use_module(cards).
:- use_module(rules).
:- use_module(possible_hands).

%% Modified Poker-style game

% poker(H, K, W) is true when H is a valid hand, K is the list of cards to keep for the best expected point payoff
% and W is the expected point payoff if the cards in K are picked.
poker(H, K, W):-
    valid_hand(H),
    best_replacement(H, K, W).

% Hand validity checks

% valid_hand(H) is true when H is a valid hand of cards, e.g. right length, no duplicates
valid_hand(H):-
    length(H, 5),
    no_duplicate_cards(H).

% no_duplicate_cards(H) is true when H only contains unique valid cards
no_duplicate_cards([]).
no_duplicate_cards([card(V, S)|T]) :-
    \+ member(card(V, S), T),
    no_duplicate_cards(T).

% Search for best hand replacement

% best_replacement(Dealt_hand, Best_hand, Max_points) is true when for a given hand, Dealt_hand, Best_hand is the
% list of cards to keep to maximize the expected number of points, Max_points
best_replacement(Dealt_hand, Best_hand, Max_points) :-
    possible_hands(Dealt_hand, Hands),
    best_replacement_helper(Hands, _, Best_hand, -1.0Inf, Max_points).

% best_replacement_helper(Hands, Current_best_hand, Total_best_hand, Current_max_points, Max_points) is true when
% for a list of card discards and their resulting search spaces, Current_best_hand is the best hand replacement
% we've seen so far, Total_best_hand is the best hand replacement for the whole list, Current_max_points is the
% highest expected point payoff we've seen so far, and Max_points is the highest expected point payoff for the entire list
best_replacement_helper([], Best_hand, Best_hand, Max_points, Max_points).
best_replacement_helper([[Hand| Search_space]|T], Hand0, Best_hand, Max0, Max_points) :-
    explore_hands(Hand, Sum),
    Expected_points is Sum / Search_space,
    (Expected_points > Max0 ->
        best_replacement_helper(T, Hand, Best_hand, Expected_points, Max_points)
        ; best_replacement_helper(T, Hand0, Best_hand, Max0, Max_points)
    ).

% Find score of each hand replacement

% explore_hands (H, S) is true when H is a valid hand replacement
% and S is a sum of all possible scores > 0 that the replacement can result in
explore_hands(H, S):-
    findall(S, find_score(H, S), L),
    sum_list(L, S).

% find_score(H, M) is true when H is a valid hand and M is the highest
% combo score that can result from that hand
find_score([card(V1,S1),card(V2,S2),card(V3,S3),card(V4,S4),card(V5,S5)] , M):-
    card(V1, S1),
    card(V2, S2),
    card(V3, S3),
    card(V4, S4),
    card(V5, S5),
    findall(S, value([card(V1,S1),card(V2,S2),card(V3,S3),card(V4,S4),card(V5,S5)], S), R),
    max_list(R, M),
    valid_hand([card(V1,S1),card(V2,S2),card(V3,S3),card(V4,S4),card(V5,S5)]).

% Test cases

% general functionality

% poker([card(10, spades), card(jack, spades), card(queen, spades),card(king, spades), card(ace, spades)], S, W).

% poker([card(10, spades), card(jack, spades), card(queen, spades), card(king, spades), card(3, hearts)], S, W).

% should keep the 4 of spades because getting a flush is more likely
% than going for a royal straight flush
% poker([card(10, spades), card(jack, spades), card(queen,spades),card(4, spades), card(3, hearts)], S, W).

% poker([card(4, hearts), card(jack, spades),card(queen,spades),card(king, spades), card(ace, spades)], S, W).

% poker([card(10, spades), card(jack, spades), card(queen,spades),card(4, hearts), card(3, hearts)], S, W).

