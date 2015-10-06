% Base de faits

init(
[
[1,1,0,0,0,-1,-1,-1,-1],
[1,1,0,0,0,0,-1,-1,-1],
[1,1,1,0,0,0,0,-1,-1],
[1,1,1,0,0,0,0,2,-1],
[1,1,1,0,0,0,2,2,2],
[-1,1,0,0,0,0,2,2,2],
[-1,-1,0,0,0,0,2,2,2],
[-1,-1,-1,0,0,0,0,2,2],
[-1,-1,-1,-1,0,0,0,2,2]
]).

% 
%jouerCoup(Joueur, OldBoard, NewBoard) :-
%

%
% Compte le nombre d'�l�ments dans une liste
%
count(_, [], 0) :- !. /* empty list, base case */
count(X, [X|T], N) :- /* if X is in the head of the list */
    count(X, T, N2), /* count on the tail (let this N2) */
    N is N2 + 1.     /* and N is N2 + 1  */
count(X, [Y|T], N) :- 
    X \= Y,          /* if X is not in the head */
    count(X, T, N).  /* just count the rest */

%
%	Condition de fin (GAME OVER !!!) 
%
gameOver() :- flatten(Board, InlineBoard),
	count(1, InlineBoard, N) = 8.
gameOver() :- flatten(Board, InlineBoard),
	count(2, InlineBoard, N) = 8.
	
%
% Boucle principale
%
play() :- gameOver, !.
play(Winner) :-
	init(Board),
	flatten(Board, InlineBoard),
	count(1, InlineBoard, N1),
	count(2, InlineBoard, N2),
	Winner is N1 + N2.



