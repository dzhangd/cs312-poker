:- module(possible_hands, [possible_hands/2]).

% possible_hands(H, R) is true when R is a list of all possible variations of H with
% cards removed
possible_hands(H, R) :-
	findall(R1, h(H, R1), R).

% No substitutions
h([C1, C2, C3, C4, C5], [C1, C2, C3, C4, C5]).

% One substitution
h([_, C2, C3, C4, C5], [_, C2, C3, C4, C5]).
h([C1, _, C3, C4, C5], [C1, _, C3, C4, C5]).
h([C1, C2, _, C4, C5], [C1, C2, _, C4, C5]).
h([C1, C2, C3, _, C5], [C1, C2, C3, _, C5]).
h([C1, C2, C3, C4, _], [C1, C2, C3, C4, _]).

% Two substitutions
h([_, _, C3, C4, C5], [_, _, C3, C4, C5]).
h([_, C2, _, C4, C5], [_, C2, _, C4, C5]).
h([_, C2, C3, _, C5], [_, C2, C3, _, C5]).
h([_, C2, C3, C4, _], [_, C2, C3, C4, _]).
h([C1, _, _, C4, C5], [C1, _, _, C4, C5]).
h([C1, _, C3, _, C5], [C1, _, C3, _, C5]).
h([C1, _, C3, C4, _], [C1, _, C3, C4, _]).
h([C1, C2, _, _, C5], [C1, C2, _, _, C5]).
h([C1, C2, _, C4, _], [C1, C2, _, C4, _]).
h([C1, C2, C3, _, _], [C1, C2, C3, _, _]).

% Three substitutions
h([_, _, _, C4, C5], [_, _, _, C4, C5]).
h([_, _, C3, _, C5], [_, _, C3, _, C5]).
h([_, _, C3, C4, _], [_, _, C3, C4, _]).
h([_, C2, _, _, C5], [_, C2, _, _, C5]).
h([_, C2, _, C4, _], [_, C2, _, C4, _]).
h([_, C2, C3, _, _], [_, C2, C3, _, _]).
h([_, C2, _, C4, _], [_, C2, _, C4, _]).
h([C1, _, _, _, C5], [C1, _, _, _, C5]).
h([C1, _, _, C4, _], [C1, _, _, C4, _]).
h([C1, C2, _, _, _], [C1, C2, _, _, _]).

% Four substitutions
h([C1, _, _, _, _], [C1, _, _, _, _]).
h([_, C2, _, _, _], [_, C2, _, _, _]).
h([_, _, C3, _, _], [_, _, C3, _, _]).
h([_, _, _, C4, _], [_, _, _, C4, _]).
h([_, _, _, _, C5], [_, _, _, _, C5]).

% Five substitutions
h([_, _, _, _, _], [_, _, _, _, _]).
