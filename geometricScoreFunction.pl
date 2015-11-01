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

:- use_module(board).

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
%%
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
%%
%% @param BaryLineBlack barycentre du noir selon les lignes
%% @param BaryColBlack barycentre du noir selon les colonnes
%% @param BaryLineWhite barycentre du blanc selon les lignes
%% @param BaryColWhite barycentre du blanc selon les colonnes
%% @param AimPointLine les coordonnées du point objectif selon les lignes
%% @param AimPointCol les coordonnées du point objectif selon les colonnes
getAimPoint(BaryLineBlack, BaryColBlack, BaryLineWhite, BaryColWhite, AimPointLine, AimPointCol) :-
    AimPointLine is (BaryLineBlack + BaryLineWhite + 4.5)/3,
    AimPointCol is (BaryColBlack + BaryColWhite + 4.5)/3.
    
%% Calcul de la distance de Manhattan entre une bille et le point visé    
%%
%% @param Line0 : coordonnées du point 0 en ligne
%% @param Col0 : coordonnées du point 0 en colonnes
%% @param Line1 : coordonnées du point 1 en colonnes
%% @param Col1 : coordonnées du point 1 en colonnes
%%
%% EXPLICATION DE L'ALGO
%%
%% Si les deux points sont sur la même ligne, alors la différence sur
%% les colonnes donne la distance
%% Sinon Si ils sont sur la même colonne, alors on compte le nombre de lignes
%% Sinon, si ils sont sur la même diagonale, alors on compte le nombre 
%% de cases qui les sépare en ligne ou en colonne
%% Sinon, on regarde la postion du poit par rapport à l'autre
%% 3 cas : 
%%         le pt1 est en bas à droite par rapport au pt0
%%       --> On refait appel à computeDistance en déplaçant le point de départ
%%           et en incrémentant OldResult
%%         le pt1 est en haut à gauche par rapport au pt0
%%       --> On refait appel à computeDistance en déplaçant le point de départ
%%           et en incrémentant OldResult
%%         le pt1 est en haut à droite ou en bas à droite (diagonale interdite)
%%        par rapport au pt0
%%       --> on calcule la distance qui les sépare en faisant
%%            D = |x1 - x0| + |y1 - y0|
computeDistance(Line0, Col0, Line1, Col1, OldResult, NewResult):- 
    (
        (
            Line0 == Line1,
            Temp is abs(Col0 - Col1),
            NewResult is (OldResult + Temp) 
        );
        (
            Col0 == Col1,
            Temp is abs(Line0 - Line1),
            NewResult is (OldResult + Temp) 
        );
        (
            D0temp is (Col0 - Line0),
            D1temp is (Col1 - Line1),
            D0temp == D1temp,
            Temp is abs(Line1 - Line0),
            NewResult is (OldResult + Temp)
        );
        (
            (
                Sline is sign(Line1 - Line0),
                Scol is sign(Col1 - Col0),
                Sline == 1,
                Scol == 1,
                Result is OldResult + 1,
                L0 is (Line0 + 1),
                C0 is (Col0 + 1),
                computeDistance(L0, C0, Line1, Col1, Result, NewResult)
            );
            (
                Sline is sign(Line1 - Line0),
                Scol is sign(Col1 - Col0),
                Sline == (-1),
                Scol == (-1),
                Result is OldResult + 1,
                L0 is (Line0 - 1),
                C0 is (Col0 - 1),
                computeDistance(L0, C0, Line1, Col1, Result, NewResult)
            );
            (
                Distline is abs(Line1 - Line0),
                Distcol is abs(Col1 - Col0),
                TempSum is (Distcol + Distline),
                NewResult is (OldResult + TempSum)
            )
        )
    ).


%% vrai si sign(A) == sign(B)
compareSign(A, B):-
    X is sign(A),
    Y is sign(B),
    X == Y.

%% Retourne le score pour une bille donnée
%% et un joueur donné

computeScoreDistance(Player, Board, AimPointLine, AimPointCol, Score):- 
    teamMarbleCoord(Player, Board, Line, Col),
    computeDistance(AimPointLine, AimPointCol, Line, Col, 0, Score).
    
%% Retourne le nombre de billes en dehors du plateau pour un joueur
%% donné, multiplié par 10 (arbitraire).
%% ceci donne le score des billes en dehors du plateau

scoreMarblesOut(Player, Board, Score, Aggressiveness):-
	flatten(Board, BoardList),
    count(Player, BoardList, N),  
    Score is (N * (-1 * Aggressiveness)).

%% Retourne le score total dans Result pour un Player donné

computeTotalScoreDistance(Player, Board, AimPointLine, AimPointCol, Result, Aggressiveness):- 
    findall(Score, computeScoreDistance(Player, Board, AimPointLine, AimPointCol, Score), Scores),
    sum_list(Scores, ScoreIn),
    scoreMarblesOut(Player, Board, ScoreOut, Aggressiveness),
    Result is (ScoreIn + ScoreOut).
    
%% On calcule la différence entre les deux scores, et cela nous donne la valeur de 
%% notre heuristique pour ce plateau et pour la couleur examinée.
%% Le meilleur score est le plus petit.
%% Je suis noir. Pour savoir si un coup est bon, il faut que je cherche à maximiser 
%% Sb - Sn (= les blancs doivent avoir un score énorme, et moi un petit score)
compareScore(Player, ScoreWhite, ScoreBlack, FinalScore):-
    (
        (
            Player == 1,
            FinalScore is (ScoreWhite - ScoreBlack)
        );
        (
            Player == 2,
            FinalScore is (ScoreBlack - ScoreWhite)        
        )
    ).


computeRandomScore(ScoreRandom):-
	random_between(-100, 100, ScoreRandom).

computeMarbleScore(Player, Board, Aggressiveness, ScoreRandom, FinalScore):-
	scoreMarblesOut(Player, Board, ScoreOut, Aggressiveness),
	FinalScore is (ScoreOut + ScoreRandom).


%% Trouve le score calculé selon l'heuristique
%% @param Board le plateau de jeu à analyser
geometricScore(Board, Player, FinalScore) :-
	geometricScore(Board, Player, FinalScore, 1000).


geometricScore(Board, Player, FinalScore, Aggressiveness) :-
	geometricScore(Board, Player, FinalScore, Aggressiveness, 0).
	
geometricScore(Board, Player, FinalScore, Aggressiveness, Random) :-
	(
		(
			Random == 1,
			computeRandomScore(ScoreRandom), 
			computeMarbleScore(Player, Board, Aggressiveness, ScoreRandom, FinalScore)
		);
		(

			Random == 0,

		    %% 1/ Calcul du centre de masse des deux couleurs BaryLine/ColBlack et BaryLine/ColWhite
		    getBarycenter(1, Board, BaryLineBlack, BaryColBlack),
		    getBarycenter(2, Board, BaryLineWhite, BaryColWhite),
		    
		    %% 2/ Calcul de la moyenne R des barycentres et du centre du plateau Cp
		    getAimPoint(BaryLineBlack, BaryColBlack, BaryLineWhite, BaryColWhite, AimPointLine, AimPointCol),
		    
		    %% 3/ Calcul des distances de Manhattan
		    computeTotalScoreDistance(1, Board, AimPointLine, AimPointCol, ScoreDistanceBlack, Aggressiveness),
		    computeTotalScoreDistance(2, Board, AimPointLine, AimPointCol, ScoreDistanceWhite, Aggressiveness),
		    
		    %% 4/ Comparaison des deux scores
		    compareScore(Player, ScoreDistanceWhite, ScoreDistanceBlack, FinalScore), 
		    
		    !
		)
	).


    
    
