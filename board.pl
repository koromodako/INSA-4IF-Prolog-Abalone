%% -----------------------------------------------------------------------------
%% Module contenant les prédicats utilitaires concernant le plateau de jeu
:- module(board, [squareContains/4, isOut/3]).
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