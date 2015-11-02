%% -----------------------------------------------------------------------------
%% Module permettant de jouer à abalone en utilisant les IA
:- module(play, []).
%% -----------------------------------------------------------------------------

:- use_module('modules/board').
:- use_module('modules/gameOver').
:- use_module('modules/ia').
:- use_module('modules/playManual').

%% Joue le tour du joueur passé en argument
%% @param Board Plateau de jeu
%% @param Player Le joueur qui doit jouer son tour
%% @param I Variable temporaire, pour limiter le nombre de boucles (dev)
play(Board, Player, I) :-
    I =< 100, % Nombre d'itération maximum
    write('------------------------------'), nl,

    write('Iteration : '), write(I), nl, % DEBUG

    % On s'arrête si game over
    not(gameOver:gameOver(Player, Board)),

    write('Tour du joueur '), write(Player),
    write(' ( '), display:displayBoardPosition(Player),  write(')'), nl,

    display:displayBoard(Board),

    %iaRandom:playTurn(Board, Player, NewBoard),
    % Reconstruction de la clé pour le joueur courant
    atom_concat('player_type_', Player, PlayerTypeKey),
    (
        (
            recorded(PlayerTypeKey, human), playManual:playTurnManually(Board, Player, NewBoard)
        )
        ;
        (
            recorded(PlayerTypeKey, computer), ia:playTurn(Board, Player, NewBoard)
        )
    ),

    display:displayBoard(NewBoard),

    NextI is I+1,
    % Tour suivant en changeant de joueur
    %sleep(1),
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
    write('Fin de la partie !'), nl,
    write('Le joueur '), write(Player), write(' a perdu.'), nl.

%% Lance le jeu après avoir demandé le mode et initialisé le plateau
play :-
    gameType,
    board:initBoard(Board),
    play(Board, 1, 1).

%% Demande à l'utilisateur quel mode jeu doit être lancé
gameType:-
		% Réinitialisation des clé/valeur sur le type de joueur.
        foreach(recorded('player_type_1', _, Ref1), erase(Ref1)),
        foreach(recorded('player_type_2', _, Ref2), erase(Ref2)),
        
        write('------------------------------'), nl,
        write('Sélection du type de jeu :'), nl,
        write('  1: '), possiblTypeOfGame(49,_,_), nl,
        write('  2: '), possiblTypeOfGame(50,_,_), nl,
        write('  3: '), possiblTypeOfGame(51,_,_), nl,
        write('Choix '),
        repeat, get_code(Choice), Choice >= 49, Choice =< 51, !,
        write('Lancement du mode : '), possiblTypeOfGame(Choice, Player1, Player2), nl,
        
        % Enregistrement du type de chaque joueur
        recorda(player_type_1, Player1), recorda(player_type_2, Player2).

%% Choix possible pour le mode de jeu
%% @param Choice Valeur ASCII donnée par l'utilisateur
%% @param Player1 Type de jeu pour le joueur 1
%% @param Player2 Type de jeu pour le joueur 2
possiblTypeOfGame(49,human,computer):- write('Humain VS Ordinateur').
possiblTypeOfGame(50,human,human):- write('Humain VS Humain').
possiblTypeOfGame(51,computer,computer):- write('Ordinateur VS Ordinateur').