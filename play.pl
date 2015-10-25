%% -----------------------------------------------------------------------------
%% Module permettant de jouer à abalone en utilisant les IA
:- module(play, []).
%% -----------------------------------------------------------------------------

:- use_module('board.pl').
:- use_module('gameOver.pl').
:- use_module('ia.pl').
:- use_module('display.pl').

%% Joue le tour du joueur passé en argument
%% @param Board Plateau de jeu
%% @param Player Le joueur qui doit jouer son tour
%% @param I Variable temporaire, pour limiter le nombre de boucles (dev)
play(Board, Player, I) :-
    I =< 10,
    print('------------------------------'), nl,

    print('Iteration : '), print(I), nl, % DEBUG

    % On s'arrête si game over
    not(gameOver:gameOver(Player, Board)),

    print('Tour du joueur '), print(Player),
    print(' ( '), display:displayBoardPosition(Player),  print(')'), nl,

    display:displayBoard(Board),

    %iaRandom:playTurn(Board, Player, NewBoard),
    (
        (
            player(Player, human), playTurnManually(Board, Player, NewBoard)
        )
        ;
        (
            player(Player, computer), ia:playTurn(Board, Player, NewBoard)
        )
    ),

    display:displayBoard(NewBoard),

    NextI is I+1,
    % Tour suivant en changeant de joueur
    sleep(1),
    (
        (
            Player == 1, play(NewBoard, 2, NextI)
        )
        ;
        (
            Player == 2, play(NewBoard, 1, NextI)
        )
    )
    ;
    print('Fin de la partie !'), nl,
    print('Le joueur '), print(Player), print(' a perdu.'), nl.

%% Lance le jeu après avoir demandé le mode et initialisé le plateau
play :-
    gameType,
    board:initBoard(Board),
    play(Board, 1, 1).

%% Demande à l'utilisateur quel mode jeu doit être lancé
gameType:-
        print('------------------------------'), nl,
        print('Sélection du type de jeu :'), nl,
        print('  1: '), possiblTypeOfGame(49,_,_), nl,
        print('  2: '), possiblTypeOfGame(50,_,_), nl,
        print('  3: '), possiblTypeOfGame(51,_,_), nl,
        print('Choix '),
        repeat, get_code(Choice), Choice>=49, Choice=<51,
        print('Lancement du mode : '), possiblTypeOfGame(Choice,Player1,Player2), nl,
        assert(player(1,Player1)), assert(player(2,Player2)).

%% Choix possible pour le mode de jeu
%% @param Choice Valeur ASCII donnée par l'utilisateur
%% @param Player1 Type de jeu pour le joueur 1
%% @param Player2 Type de jeu pour le joueur 2
possiblTypeOfGame(49,human,computer):- print('Humain VS Ordinateur').
possiblTypeOfGame(50,human,human):- print('Humain VS Humain').
possiblTypeOfGame(51,computer,computer):- print('Ordinateur VS Ordinateur').

%% Joue le tour en interactif du joueur passé en argument.
%% @param Board Plateau de jeu
%% @param Player Le joueur qui doit jouer son tour
playTurnManually(Board, Player, NewBoard):-
    print('Sélection une bille à déplacer, donner la ligne (ex. 3) '),
    repeat, get_code(ChoiceLine), ChoiceLine>=49, ChoiceLine=<57, Line is ChoiceLine-48,
    print('Donner la colonne (ex. D) '),
    repeat, get_code(ChoiceCol), ChoiceCol>=65, ChoiceCol=<73, Col is ChoiceCol-64,
    print('Liste des déplacements possibles :'), nl,
    findall(
       [NextLine, NextCol], 
       (
           movable:playerMovements(Board, Player, Line, Col, NextLine, NextCol)
       ),
       NewMovements
    ),
    NewBoard is Board, % A supprimer
    print(NewMovements) % A supprimer
    % Afficher toutes les possibilités
.