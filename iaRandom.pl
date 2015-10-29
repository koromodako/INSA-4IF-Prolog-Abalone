%% -----------------------------------------------------------------------------
%% Module contenant l'IA réalisant des déplacements aléatoires
:- module(iaRandom, [playTurn/3]).
%% -----------------------------------------------------------------------------

:- use_module(board).
:- use_module(movable).
:- use_module(move).
:- use_module(display).

%% Trouve le prochaine déplacement à effectuer, de façon aléatoire
%% @param Board Plateau de jeu
%% @param Player Le joueur qui doit jouer son tour
%% @param Line La ligne (basée 1) de la case de départ
%% @param Col La colonne (basée 1) de la case de départ
%% @param LineDest La ligne (basée 1) de la case d'arrivée
%% @param ColDest La colonne (basée 1) de la case d'arrivée
nextMove(Board, Player, Line, Col, NextLine, NextCol) :-

    % On récupère la taille du plateau
    length(Board, Size),
    MaxSize is Size + 1,

    % On réessaye de trouver un déplacement tant qu'on en possède pas un 
    % de faisable.
    repeat,

    % On tire au sort une case
    random(1, MaxSize, Line), random(1, MaxSize, Col),
    
    % On vérifie qu'elle appartient bien au joueur
    board:squareContains(Board, Line, Col, Player),

    % On tire au sort la direction
    random(-1, 2, DiffLine), random(-1, 2, DiffCol),
    not((DiffLine == 0, DiffCol == 0)),

    NextLine is Line + DiffLine, NextCol is Col + DiffCol,

    % On vérifie que le déplacement est possible
    movable:isMovable(Board, Line, Col, NextLine, NextCol),

    !.

%% Joue le tour du joueur passé en argument.
%% @param Board Plateau de jeu
%% @param Player Le joueur qui doit jouer son tour
playTurn(Board, Player, NewBoard) :-
    %display:displayBoard(Board),
    nextMove(Board, Player, Line, Col, NextLine, NextCol),
    print('Line : '), print(Line),nl,
    print('Col : '),print(Col),nl,
    print('NextLine : '),print(NextLine),nl,
    print('NextCol : '),print(NextCol),nl,
    moveMarbles(Board, Col, Line, NextCol, NextLine, NewBoard).
    %display:displayBoard(Board).
    
    
