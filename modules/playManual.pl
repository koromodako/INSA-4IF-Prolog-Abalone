%% -----------------------------------------------------------------------------
%% Module permettant de jouer de manière interactive avec l'utilisateur
:- module(playManual, [playTurnManually/3]).
%% -----------------------------------------------------------------------------

:- use_module('display.pl').
:- use_module('move.pl').
:- use_module('movable.pl').

%% Joue le tour en interactif du joueur passé en argument.
%% @param Board Plateau de jeu
%% @param Player Le joueur qui doit jouer son tour
playTurnManually(Board, Player, NewBoard):-
    write('Sélection une bille à déplacer, donner la ligne (ex. 3) '),
    repeat,
    (
        repeat, get_code(ChoiceLine), ChoiceLine >= 49, ChoiceLine =< 57, !,
        Line is ChoiceLine-48, 
        write('Donner la colonne (ex. D) '),
        repeat, get_code(ChoiceCol), ChoiceCol >= 65, ChoiceCol =< 73, !,
        Col is ChoiceCol-64
    ), !, % Il faut vérifier ici que c'est une bille correspondant au joueur
    write('Liste des déplacements possibles :'), nl,
    findall(
       [NextLine, NextCol], 
       (
           movable:playerMovements(Board, Player, Line, Col, NextLine, NextCol)
       ),
       NewMovements
    ),
    display:displayMovements(Line, Col, NewMovements),
    write('Sélection le déplacement '), length(NewMovements, NumberOfMovements),
    MaxChoiceMovement is NumberOfMovements + 48,
    repeat, get_code(ChoiceMovement), ChoiceMovement >= 49, ChoiceMovement =< MaxChoiceMovement, !,
    IndexMovement is ChoiceMovement - 48,
    nth1(IndexMovement,NewMovements,NextLineCol),
    nth1(1, NextLineCol, NextLine), nth1(2, NextLineCol, NextCol),
    move:moveMarbles(Board, Col, Line, NextCol, NextLine, NewBoard).
