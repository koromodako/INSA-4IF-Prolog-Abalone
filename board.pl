%% -----------------------------------------------------------------------------
%% Module contenant les prédicats utilitaires concernant le plateau de jeu
:- module(board, [squareContains/4, isOut/3, count/3, initBoard/1]).
%% -----------------------------------------------------------------------------

%% Vérifie si le plateau "Board" contient la valeur "Value" à l'emplacement
%% (Line, Col).
squareContains(Board, Line, Col, Value) :-
	flatten(Board, BoardList),
	I is (Line - 1) * 9 + Col,
	nth1(I, BoardList, Value).

%% Vérifie si la case (Line, Col) correspond à un emplacement en dehors du 
%% plateau de jeu.
isOut(Board, Line, Col) :- 
	not(
		(
			% Case existante dans la matrice
			between(0, 9, Line), between(0, 9, Col), 
			
			% Vérifie si la case existe, ie. valeur != -1 
			not(squareContains(Board, Line, Col, -1))
		)
	).

%%
%% Compte le nombre d'occurences d'un élément dans une liste
%% (fonctionnel)
%%
count(_, [], 0) :- !. /* empty list, base case */
count(X, [X|T], N) :- /* if X is in the head of the list */
    count(X, T, N2), /* count on the tail (let this N2) */
    N is N2 + 1.     /* and N is N2 + 1  */
count(X, [Y|T], N) :- 
    X \= Y,          /* if X is not in the head */
    count(X, T, N).  /* just count the rest */
    
%% Initiale le plateau
initBoard(
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