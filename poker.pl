:- use_module(cards).
:- use_module(rules).

%% Modified Poker-style game

% ! Poker (H, K) where H is a list of 5 cards and K is a list of =< 5
% cards to keep from H in order to maximize your win
%
poker(H, K):-
    valid_hand(H),
    keep(H, K).

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

%%%%%%%%%%%%%%%

% keep(H, K) finds the best cards to keep K from hand H
% TODO: how do we do this

% Case: all 5 cards already result in a combo
keep(H, H) :-
    value(H, 250).
keep(H, H) :-
    value(H, 25).
keep(H, H) :-
    value(H, 10).
keep(H, H) :-
    value(H, 10).
keep(H, H) :-
    value(H, 10).

% Case: hand cannot result in points
keep(H, []) :-
    \+ value(H, _).

% poker([card(ace, spades), card(10, clubs), card(7, spades), card(queen, spades), card(5, diamonds)], K).
% K = [].

%%%%%%%%%%%%%%%

% Hand sorting helper makes it easier to recognize combos
% https://stackoverflow.com/questions/11852226/sort-a-list-of-cards-prolog

compare_values(D, card(A,_), card(B,_)) :-
    nth0(X, [2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king, ace], A),
    nth0(Y, [2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king, ace], B),
    compare(D, X, Y).

sort_cards(L, R) :-
    predsort(compare_values, L, R).
