%% -----------------------------------------------------------------------------
%% Module contenant les prédicats permettant d'afficher la grille de jeu
:- module(display, [displayBoard/1]).
%% -----------------------------------------------------------------------------

%% Affiche la grille du jeu avec les indices des lignes et des colonnes
%% @param Board La grille de jeu à afficher
displayBoard(Board):-
    nl, write('       A B C D EFGHI'),
    nl, write('       _________'),
    nl, displayRows(Board, 1),!, nl.

%% Affiche les lignes de la grille de jeu
%% @param Board La grille de jeu à afficher
%% @param RowNb Numéro de ligne à afficher
displayRows([], _).
displayRows([Row|Rest], RowNb):-
    write(RowNb), write('  '),
    NbSpace is abs(5-RowNb),
    displaySpaces(NbSpace),
    displayRow(Row, 1),
    write(' '), nl,
    NextRowNb is RowNb+1,
    displayRows(Rest, NextRowNb).

%% Affiche une ligne de la grille de jeu
%% @param Row La ligne à afficher
%% @param ColNb Numéro de colonne à afficher
displayRow(_, 10).
displayRow([Element|Rest], ColNb):-
    displayPosition(Element),
    NextColNb is ColNb+1,
    displayRow(Rest, NextColNb).

%% Affiche des espaces
%% @param NbSpace Nombre d'espace à afficher
displaySpaces(0).
displaySpaces(NbSpace):-
    write(' '),
    NbSpaceLeft is NbSpace-1,
    displaySpaces(NbSpaceLeft).

%% Affiche une case de la grille
%% @param Element L'élément à afficher
displayPosition(-1).
displayPosition(Element):-
    write(Element), write(' ').