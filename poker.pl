:- use_module(cards).
:- use_module(rules).
:- use_module(possible_hands).

%% Modified Poker-style game

% ! Poker (H, K) where H is a list of 5 cards and K is a list of =< 5
% cards to keep from H in order to maximize your win

poker(H, K, W):-
    valid_hand(H),
    best_replacement(H, K, W).

%%%%%%%%%%%%%%%
%Hand Validation
%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%
%Replacement Finder
%%%%%%%%%%%%%%%%%%%
best_replacement(H, Best_hand, Max_weight) :-
	possible_hands(H, Hands),
	best_replacement_helper(Hands, Best_hand, Max_weight).

best_replacement_helper(Hands, Best_hand, Max_weight) :-
	best_replacement_helper(Hands, _, Best_hand, -1.0Inf, Max_weight).

best_replacement_helper([], Best_hand, Best_hand, Max_weight, Max_weight).
best_replacement_helper([[Hand| Search_space]|T], Hand0, Best_hand, Max0, Max_weight) :-
	explore_hands(Hand, Sum),
	Weight is Sum / Search_space,
	(Weight > Max0 ->
		best_replacement_helper(T, Hand, Best_hand, Weight, Max_weight)
		; best_replacement_helper(T, Hand0, Best_hand, Max0, Max_weight)
	).

%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
%Hand Exploration
%%%%%%%%%%%%%%%%%%
% explore_hands (H, S) is true when H is a valid hand
% and S is a sum of all possible scores > 0 from that hand

explore_hands(H, S):-
    findall(S, find_score(H, S), L),
    sum_list(L, S).

% find_score(H, M) is true when H is a valid hand a M is the highest
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

max([], 0).
max(L, M) :- max_list(L, M).

average([], 0).
average(List, Average) :- sum_list(List, Sum),
                          length(List, Count),
                          Average is Sum/Count.

%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%
% Test Cases
%%%%%%%%%%%%%%

% general functionality

% poker([card(10, spades), card(jack, spades), card(queen, spades),card(king, spades), card(ace, spades)], S, W).

% poker([card(10, spades), card(jack, spades), card(queen, spades), card(king, spades), card(3, hearts)], S, W).

% poker([card(10, spades), card(jack, spades), card(queen,spades),card(4, spades), card(3, hearts)], S, W).

% poker([card(4, hearts), card(jack, spades),card(queen,spades),card(king, spades), card(ace, spades)], S, W).

% bad types
