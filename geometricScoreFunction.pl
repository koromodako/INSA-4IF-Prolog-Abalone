%% -----------------------------------------------------------------------------
%% Heuristique 1 : billes centrées et compactes
%% 1/ Calcul du centre de masse des deux couleurs Cn et Cb
%% Pour le calculer, il faut faire la somme des coordonnées en x et y de toutes 
%% les billes présentes sur le plateau (on ignore pour l’instant les billes tombées) 
%% par couleur, puis on divise cette somme par le nombre total de billes prises en compte.
%% 
%% 2/ Calcul de la moyenne R de Cn, Cb, et du centre du plateau Cp
%% Voilà.
%% 
%% 3/ Calcul des distances de Manhattan
%% Pour chaque couleur on calcule la somme des distances de Manhattan entre chaque 
%% bille et le centre R. On compte aussi les billes sorties avec une valeur 
%% constante > distance max possible sur le plateau. On obtient deux scores : Sn et Sb.
%% 
%% 4/ Comparaison des deux scores
%% On calcule la différence entre les deux scores, et cela nous donne la valeur de 
%% notre heuristique pour ce plateau et pour la couleur examinée.
%% Le meilleur score est le plus petit.
%% Je suis noir. Pour savoir si un coup est bon, il faut que je cherche à maximiser 
%% Sb - Sn (= les blancs doivent avoir un score énorme, et moi un petit score)
 
:- module(geometricScoreFunction, [geometricScore/3]).

%% -----------------------------------------------------------------------------

:- use_module('board.pl').


	

%% Calcule un couple (Line, Col) contenant une bille de la couleur du Player
%% @param Player numéro du joueur (1 ou 2)
%% @param Board le plateau a analyser
%% @param Line la ligne de la case trouvee
%% @param Col la colonne de la case trouvee
teamMarbleCoord(Player, Board, Line, Col) :-
	between(1, 9, Col), between(1, 9, Line),
	board:squareContains(Board, Line, Col, Player).

%% Calcule le centre de masse d'une couleur (=la somme des coordonnees de l'ensemble des billes d'une couleur présentes
%% sur le plateau / nb billes d'une couleur)
%% @param Player numéro du joueur (1 ou 2)
%% @param Board le plateau a analyser
%% @param SumCol la somme des coordonnees en colonne
%% @param SumLine la somme des coordonnees en ligne
getBarycenter(Player, Board, BarycenterLine, BarycenterCol) :-
	findall(Line, teamMarbleCoord(Player, Board, Line, Col), Lines),
	sum_list(Lines, SumLine),
	length(Lines, LinesLength),
	BarycenterLine is SumLine / LinesLength,
	findall(Col, teamMarbleCoord(Player, Board, Line, Col), Cols),
	sum_list(Cols, SumCol),
	BarycenterCol is SumCol / LinesLength.

%% Calcul le point objectif du plateau en faisant la moyenne des barycentres des boules
%% des deux joueurs et du centre du plateau.
%% @param BaryLineBlack barycentre du noir selon les lignes
%% @param BaryColBlack barycentre du noir selon les colonnes
%% @param BaryLineWhite barycentre du blanc selon les lignes
%% @param BaryColWhite barycentre du blanc selon les colonnes
%% @param AimPointLine les coordonnées du point objectif selon les lignes
%% @param AimPointCol les coordonnées du point objectif selon les colonnes
getAimPoint(BaryLineBlack, BaryColBlack, BaryLineWhite, BaryColWhite, AimPointLine, AimPointCol) :-
	AimPointLine is (BaryLineBlack + BaryLineWhite + 4.5)/3,
	AimPointCol is (BaryColBlack + BaryColWhite + 4.5)/3.
	
%% Trouve le score calculé selon l'heuristique
%% @param Board le plateau de jeu à analyser
geometricScore(Board, AimPointLine, AimPointCol) :-
	board:initBoard(Board),
	
	%% 1/ Calcul du centre de masse des deux couleurs BaryLine/ColBlack et BaryLine/ColWhite
	getBarycenter(1, Board, BaryLineBlack, BaryColBlack),
	getBarycenter(2, Board, BaryLineWhite, BaryColWhite),
	
	%% 2/ Calcul de la moyenne R des barycentres et du centre du plateau Cp
	getAimPoint(BaryLineBlack, BaryColBlack, BaryLineWhite, BaryColWhite, AimPointLine, AimPointCol).