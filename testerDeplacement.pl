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
	between(0, 9, Ligne), between(0, 9, Col),
	between(0, 9, LigneDest), between(0, 9, ColDest),
	SommeOrigine is Ligne + Col, SommeDest is LigneDest + ColDest,
	SommeOrigine \== SommeDest, 
	not(caseContient(Board, LigneDest, ColDest, -1)). % Vérifie si la case de destination existe


play(Result) :-
	init(Board),
	deplacement(Board, 2, 3, 1, 4),
	Result is 0.
	

% Teste si un déplacement est autorisé




