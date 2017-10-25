:- module(possible_hands, [possible_hands/2]).

% possible_hands(H, R) is true when R is a list of all possible variations of H with cards removed
possible_hands(H, R) :-
	findall([R1|S], h(H, R1, S), R).

% h models all possible card substitutions given five cards

% No substitutions
h([C1, C2, C3, C4, C5], [C1, C2, C3, C4, C5], 1).

% One substitution (47 possible new hands)
h([_, C2, C3, C4, C5], [_, C2, C3, C4, C5], 47).
h([C1, _, C3, C4, C5], [C1, _, C3, C4, C5], 47).
h([C1, C2, _, C4, C5], [C1, C2, _, C4, C5], 47).
h([C1, C2, C3, _, C5], [C1, C2, C3, _, C5], 47).
h([C1, C2, C3, C4, _], [C1, C2, C3, C4, _], 47).

% Two substitutions (47*46 possible new hands)
h([_, _, C3, C4, C5], [_, _, C3, C4, C5], 2162).
h([_, C2, _, C4, C5], [_, C2, _, C4, C5], 2162).
h([_, C2, C3, _, C5], [_, C2, C3, _, C5], 2162).
h([_, C2, C3, C4, _], [_, C2, C3, C4, _], 2162).
h([C1, _, _, C4, C5], [C1, _, _, C4, C5], 2162).
h([C1, _, C3, _, C5], [C1, _, C3, _, C5], 2162).
h([C1, _, C3, C4, _], [C1, _, C3, C4, _], 2162).
h([C1, C2, _, _, C5], [C1, C2, _, _, C5], 2162).
h([C1, C2, _, C4, _], [C1, C2, _, C4, _], 2162).
h([C1, C2, C3, _, _], [C1, C2, C3, _, _], 2162).

% Three substitutions (47*46*45 possible new hands)
h([_, _, _, C4, C5], [_, _, _, C4, C5], 97290).
h([_, _, C3, _, C5], [_, _, C3, _, C5], 97290).
h([_, _, C3, C4, _], [_, _, C3, C4, _], 97290).
h([_, C2, _, _, C5], [_, C2, _, _, C5], 97290).
h([_, C2, _, C4, _], [_, C2, _, C4, _], 97290).
h([_, C2, C3, _, _], [_, C2, C3, _, _], 97290).
h([_, C2, _, C4, _], [_, C2, _, C4, _], 97290).
h([C1, _, _, _, C5], [C1, _, _, _, C5], 97290).
h([C1, _, _, C4, _], [C1, _, _, C4, _], 97290).
h([C1, C2, _, _, _], [C1, C2, _, _, _], 97290).

%% % Four substitutions (47*46*45*44 possible new hands)
h([C1, _, _, _, _], [C1, _, _, _, _], 4280760).
h([_, C2, _, _, _], [_, C2, _, _, _], 4280760).
h([_, _, C3, _, _], [_, _, C3, _, _], 4280760).
h([_, _, _, C4, _], [_, _, _, C4, _], 4280760).
h([_, _, _, _, C5], [_, _, _, _, C5], 4280760).

%% % Five substitutions (47*46*45*44*43 possible new hands)
h([_, _, _, _, _], [_, _, _, _, _], 184072680).
