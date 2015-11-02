%% -----------------------------------------------------------------------------
%% Module contenant les prédicats permettant de vérifier si un déplacement est
%% valide.
:- module(movable, [isMovable/5,playerMovements/6]).
%% -----------------------------------------------------------------------------

:- use_module(board).

%% Vérifie si la Strength testée est bien autorisée par les règles du jeu 
%% On ne peut déplacer plus de 3 boules
checkStrength(Strength) :- 
    Strength =< 3. 

%% Vérifie que la direction du déplacement est autorisée.
%% Seule les diagonales (x, y) -> (x+1, y-1) et (x-1, y+1) 
%% sont interdites.
%% @param Line La ligne (basée 1) de la case de départ
%% @param Col La colonne (basée 1) de la case de départ
%% @param LineDest La ligne (basée 1) de la case d'arrivée
%% @param ColDest La colonne (basée 1) de la case d'arrivée
checkDirection(Line, Col, LineDest, ColDest) :- 
    SommeOrigine is Line + Col, SommeDest is LineDest + ColDest,
    SommeOrigine \== SommeDest.

%% Lorsque la case de destination est hors plateau, vérifie s'il ne s'agit pas
%% d'un suicide (c'est-à-dire que la boule avant le vide n'est pas de la même 
%% couleur que la boule de départ), et, dans le cas contraire, si la force est
%% suffisante pour pousser la boule adverse.
%% @param InitialSquareContent Le contenu de la case d'origine
%% @param PrevSquareContent Le contenu de la case de depart
%% @param Strength La force courante du déplacement
isMovableOutOfBoard(InitialSquareContent, PrevSquareContent, Strength) :-
    (
        % Suicide interdit (contenu de la case d'origine == à la case de départ)
        (
            PrevSquareContent == InitialSquareContent,
            fail
        )
        ;
        % On vérifie qu'on peut pousser l'adversaire
        (
            PrevSquareContent \== InitialSquareContent, 
            Strength > 0
        )
    ).

%% Lorsque la case de destination est sur le plateau, vérifie si le déplacement
%% est autorisé.
%% @param Board Le plateau de jeu
%% @param InitialSquareContent Le contenu de la case d'origine
%% @param PrevSquareContent Le contenu de la case de depart
%% @param LineDest La ligne (basée 1) de la case d'arrivée
%% @param ColDest La colonne (basée 1) de la case d'arrivée
%% @param NewLine La ligne (basée 1) de la prochaine case
%% @param NewCol La colonne (basée 1) de la prochaine case
%% @param Strength La force courante du déplacement
isMovableInsideBoard(Board, InitialSquareContent, PrevSquareContent, LineDest, ColDest, NewLine, NewCol, Strength) :-
    
    % Récupération du contenu de la case de destination
    board:squareContains(Board, LineDest, ColDest, NextSquareContent),

    (
        % Si la case de destination n'est pas vide (contenu = 0 : case vide, 
        % contenu = -1 : case innexistante)
        (
            NextSquareContent > 0,
                                
            (    
                % Si la case de destination est de la même couleur que la boule origine
                (
                    NextSquareContent == InitialSquareContent,
                
                    % Si la case précédente est de la meme couleur que la case suivante 
                    % et la case origine, on augmente la force                        
                    PrevSquareContent == InitialSquareContent,
                
                    % On augmente la force en appelant de nouveau isMovable
                    isMovable(Board, InitialSquareContent, LineDest, ColDest, NewLine, NewCol, Strength + 1)   
                )
                ;
                % Si la case de destination n'est pas de la même couleur que la boule d'origine
                (
                    NextSquareContent \== InitialSquareContent, 

                    % On diminue la force et on appelle de nouveau isMovable pour tester la case
                    % suivante
                    isMovable(Board, InitialSquareContent, LineDest, ColDest, NewLine, NewCol, Strength - 1)
                )
            )
        )
        ;
        % Si la case de destination est vide, on vérifie juste la force
        (
            NextSquareContent == 0,
                                
            Strength > 0
        )
    ).

%% Teste si un déplacement est autorisé sur le plateau "Board".
%% @param Board Plateau de jeu
%% @param InitialSquareContent La valeur de la case à l'origine du déplacement
%% @param Line La ligne (basée 1) de la case de départ
%% @param Col La colonne (basée 1) de la case de départ
%% @param LineDest La ligne (basée 1) de la case d'arrivée
%% @param ColDest La colonne (basée 1) de la case d'arrivée
%% @param Strength La force accumulée pour le déplacement
isMovable(Board, InitialSquareContent, Line, Col, LineDest, ColDest, Strength) :- 

    not(board:isOut(Board, Line, Col)),

    % On vérifie que la Strength courante est bien autorisée
    checkStrength(Strength),

    % On vérifie que la case de départ est conforme
    between(1, 9, Line), between(1, 9, Col),
    
    % On vérifie la direction
    checkDirection(Line, Col, LineDest, ColDest),    

    % On récupère la valeur de la case de départ
    board:squareContains(Board, Line, Col, PrevSquareContent),

    % On calcule l'écart entre les coordonnées pour l'appel récursif, afin de
    % continuer dans la même direction
    LineDiff is LineDest - Line,
    ColDiff is ColDest - Col,
    NewLine is LineDest + LineDiff,
    NewCol is ColDest + ColDiff,

    % En fonction de ce que contient la case de destination
    (
        % Destination hors plateau
        (
            board:isOut(Board, LineDest, ColDest),
            
            isMovableOutOfBoard(InitialSquareContent, PrevSquareContent, Strength)
        )
        ;
        % Destination sur le plateau
        (
            not(board:isOut(Board, LineDest, ColDest)),
            
            isMovableInsideBoard(Board, InitialSquareContent, PrevSquareContent, LineDest, ColDest, NewLine, NewCol, Strength)
        )
    ).

%% Teste si un déplacement est autorisé sur le plateau "Board".
%% @param Board Plateau de jeu
%% @param Line La ligne (basée 1) de la case de départ
%% @param Col La colonne (basée 1) de la case de départ
%% @param LineDest La ligne (basée 1) de la case d'arrivée
%% @param ColDest La colonne (basée 1) de la case d'arrivée
isMovable(Board, Line, Col, LineDest, ColDest) :- 
    % Récupère le contenu de la case de départ
    board:squareContains(Board, Line, Col, InitialSquareContent),
     
    % Vérifie le déplacement avec une force initiale de 1
    isMovable(Board, InitialSquareContent, Line, Col, LineDest, ColDest, 1). 

%% Permet de récupérer les mouvements possibles pour le joueur
playerMovements(Board, Player, Line, Col, NextLine, NextCol) :-
    % Pour chaque case du plateau
    between(1, 9, Line), between(1, 9, Col),
    board:squareContains(Board, Line, Col, Player),
    
    % Les différentes directions (sauf x+0, y+0)
    between(-1,1,DiffLine), between(-1,1,DiffCol),
    not((DiffLine == 0, DiffCol == 0)),
    
    NextLine is Line + DiffLine, NextCol is Col + DiffCol,
    
    % On regarde si le déplacement est possible
    movable:isMovable(Board, Line, Col, NextLine, NextCol).

