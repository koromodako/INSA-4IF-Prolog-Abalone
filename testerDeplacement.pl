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

caseContient(Board, Ligne, Col, Valeur) :-
	flatten(Board, BoardList),
	I is (Ligne - 1) * 9 + Col,
	nth1(I, BoardList, Valeur).

deplacement(Board, Ligne, Col, LigneDest, ColDest) :- 
	Ligne >= 0, Col >= 0, LigneDest >= 0, ColDest >= 0,
	Ligne =< 9, Col =< 9, LigneDest =< 9, ColDest =< 9,
	not(caseContient(Board, LigneDest, ColDest, -1)). % Vérifie si la case de destination existe


play(Result) :-
	init(Board),
	deplacement(Board, 1, 5, 1, 6),
	Result is 0.
	

% Teste si un déplacement est autorisé




