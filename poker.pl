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


%%%%%%%%%%%%%%%

%!Rules
% Assume sorted hand

% Royal Straight Flush : 250pt
value([card(10,X),card(jack,X),card(queen,X),card(king,X),card(ace,X)], 250).

% Five of a Kind: 60pt 
% TODO: only if we decide to use jokers

% Straight Flush: 25pt
% TODO: determine if its a straight

% Four of a Kind : 20pt
value([_,card(A,_),card(A,_),card(A,_),card(A,_)], 20).
value([card(A,_),card(A,_),card(A,_),card(A,_),_], 20).

% Full House : 10 pt
value([card(A,_),card(A,_),card(A,_),card(B,_),card(B,_)], 10).
value([card(A,_),card(A,_),card(B,_),card(B,_),card(B,_)], 10).

% Flush : 4pt
value([card(_,X),card(_,X),card(_,X),card(_,X),card(_,X)], 4).

% Straight : 3pt
% TODO: how do we determine if its a straight

% Three of a Kind : 1pt
value([_,card(A,_),card(A,_),card(A,_),_], 1).
value([_,_,card(A,_),card(A,_),card(A,_)], 1).
value([card(A,_),card(A,_),card(A,_),_,_], 1).

% Two Pairs : 1pt
value([card(A,_),card(A,_),card(B,_),card(B,_),_], 1).
value([_,card(A,_),card(A,_),card(B,_),card(B,_)], 1).
value([card(A,_),card(A,_),_,card(B,_),card(B,_)], 1).

% No Combo
value(_, 0).

%%%%%%%%%%%%

% Hand sorting makes it easier to recognize combos
% https://stackoverflow.com/questions/11852226/sort-a-list-of-cards-prolog

compare_values(D, card(A,_), card(B,_)) :-
    nth0(X, [2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king, ace], A),
    nth0(Y, [2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king, ace], B),
    compare(D, X, Y).

sort_cards(L, R) :-
    predsort(compare_values, L, R).