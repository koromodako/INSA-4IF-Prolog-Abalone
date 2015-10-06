
init(
[
[ 1, 1, 0, 0, 0,-1,-1,-1,-1],
[ 1, 1, 0, 0, 0, 0,-1,-1,-1],
[ 0, 0, 1, 1, 1, 2, 2,-1,-1],
[ 0, 1, 0, 1, 1, 2, 2, 0,-1],
[ 1, 1, 1, 0, 0, 0, 2, 2, 0],
[-1, 1, 0, 0, 0, 0, 2, 2, 0],
[-1,-1, 0, 0, 0, 0, 2, 2, 2],
[-1,-1,-1, 0, 0, 0, 0, 2, 2],
[-1,-1,-1,-1, 0, 0, 0, 2, 2]
]).


caseContient(Board, Ligne, Col, Valeur) :-
	flatten(Board, BoardList),
	I is (Ligne -1) * 9 + Col,
	nth1(I, BoardList, Valeur).

isOut(Board, Ligne, Col) :- 
	between(0, 9, Ligne), between(0, 9, Col),
	not(caseContient(Board, Ligne, Col, -1)). % Vérifie si la case existe

forceAutorisee(Force) :- Force =< 3. 

deplacement(Board, CouleurOrigine, Ligne, Col, LigneDest, ColDest, Force) :- 

	% On ne peut déplacer plus de 3 boules
	forceAutorisee(Force),

	between(0, 9, Ligne), between(0, 9, Col),
	
	% La diagonale (x,z) -> (x+1,z-1) est interdite
	SommeOrigine is Ligne + Col, SommeDest is LigneDest + ColDest,
	SommeOrigine \== SommeDest,

	% Vérifie si la destination est bien sur le plateau 
	% isOut(Board, LigneDest, ColDest),

	% On calcule l écart entre les coordonnées pour l appel récursif
	EcartLigne is LigneDest - Ligne,
	EcartCol is ColDest - Col,
	NouvelleLigne is LigneDest + EcartLigne,
	NouvelleCol is ColDest + EcartCol,

	% En fonction de ce que contient la case de destination
	(
		% Si la case de destination est vide ou n existe pas
		(
			(isOut(Board, LigneDest, ColDest) ; caseContient(Board, LigneDest, ColDest, 0)),
			% On vérifie que la force est positive : moins de boules adversaires en face
			Force > 0 
		)
		;
		% Si la case de destination n est pas vide
		(
			% Si la case de destination est de la même couleur que la boule origine
			(
				caseContient(Board, LigneDest, ColDest, CouleurOrigine),
				
				% On augmente la force
				deplacement(Board, CouleurOrigine, LigneDest, ColDest, NouvelleLigne, NouvelleCol, Force + 1)
			)
			;
			% Si la case de destination n est pas de la même couleur que la boule origine
			(
				% On diminue la force
				deplacement(Board, CouleurOrigine, LigneDest, ColDest, NouvelleLigne, NouvelleCol, Force - 1)
			)
		)
	).


play(Result) :-
	init(Board),
	deplacement(Board, 1, 3, 3, 3, 4, 1),
	Result is 0.
	

% Teste si un déplacement est autorisé




