%% -----------------------------------------------------------------------------
%% Module contenant les prédicats permettant d'afficher la grille de jeu
:- module(display, [displayBoard/1]).
%% -----------------------------------------------------------------------------

%% Affiche la grille du jeu avec les indices des lignes et des colonnes
%% @param Board La grille de jeu à afficher
displayBoard(Board):-
    nl, write('           A B C D E'),
    nl, write('          / / / / /  F'),
    nl, write('        ─────────   / G'),
    nl, displayRows(Board, 1),!,
    write('        ─────────'), nl.

%% Affiche les lignes de la grille de jeu
%% @param Board La grille de jeu à afficher
%% @param RowNb Numéro de ligne à afficher
displayRows([], _).
displayRows([Row|Rest], RowNb):-
    NbSpace is abs(5-RowNb),
    displaySpaces(NbSpace),
    write(RowNb), write('─'),
    displaySeparatorBegin(RowNb), write(' '),
    displayRow(Row, 1),
    displaySeparatorEnd(RowNb), nl,
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

%% Affiche le caractère pour délimiter la grille (début de ligne)
%% @param RowNb Numéro de ligne
displaySeparatorBegin(RowNb):-
    RowNb < 5, write('/');
    RowNb == 5, write('|');
    RowNb > 5, write('\\').

%% Affiche le caractère pour délimiter la grille (fin de ligne)
%% @param RowNb Numéro de ligne
displaySeparatorEnd(RowNb):-
    RowNb == 1, write('\\  / H');
    RowNb == 2, write('\\  / I');
    RowNb == 3, write('\\  /');
    RowNb < 5, write('\\');
    RowNb == 5, write('|');
    RowNb > 5, write('/').

%% Affiche une case de la grille
%% @param Element L'élément à afficher
displayPosition(-1).
displayPosition(Element):-
    Element == -1;
    Element == 0, write('.'), write(' ');
    Element == 1, write('x'), write(' ');
    Element == 2, write('o'), write(' ').