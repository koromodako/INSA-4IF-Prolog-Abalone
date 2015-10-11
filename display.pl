%% -----------------------------------------------------------------------------
%% Module contenant les prédicats permettant d'afficher la grille de jeu
:- module(display, [displayBoard/1, displayMatrix/1]).
%% -----------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Display Board

%% Affiche la grille du jeu avec les indices des lignes et des colonnes
%% @param Board La grille de jeu à afficher
displayBoard(Board):-
    nl, write('           A B C D E'),
    nl, write('          / / / / /  F'),
    nl, write('        ─────────   / G'),
    nl, displayBoardRows(Board, 1),!,
    write('        ─────────'), nl.

%% Affiche les lignes de la grille de jeu
%% @param Board La grille de jeu à afficher
%% @param RowNb Numéro de ligne à afficher
displayBoardRows([], _).
displayBoardRows([Row|Rest], RowNb):-
    NbSpace is abs(5-RowNb),
    displaySpaces(NbSpace),
    write(RowNb), write('─'),
    displayBoardSeparatorBegin(RowNb), write(' '),
    displayBoardRow(Row, 1),
    displayBoardSeparatorEnd(RowNb), nl,
    NextRowNb is RowNb+1,
    displayBoardRows(Rest, NextRowNb).

%% Affiche une ligne de la grille de jeu
%% @param Row La ligne à afficher
%% @param ColNb Numéro de colonne à afficher
displayBoardRow(_, 10).
displayBoardRow([Element|Rest], ColNb):-
    displayBoardPosition(Element),
    NextColNb is ColNb+1,
    displayBoardRow(Rest, NextColNb).

%% Affiche le caractère pour délimiter la grille (début de ligne)
%% @param RowNb Numéro de ligne
displayBoardSeparatorBegin(RowNb):-
    RowNb < 5, write('/');
    RowNb == 5, write('|');
    RowNb > 5, write('\\').

%% Affiche le caractère pour délimiter la grille (fin de ligne)
%% @param RowNb Numéro de ligne
displayBoardSeparatorEnd(RowNb):-
    RowNb == 1, write('\\  / H');
    RowNb == 2, write('\\  / I');
    RowNb == 3, write('\\  /');
    RowNb < 5, write('\\');
    RowNb == 5, write('|');
    RowNb > 5, write('/').

%% Affiche une case de la grille
%% @param Element L'élément à afficher
displayBoardPosition(-1).
displayBoardPosition(Element):-
    Element == -1;
    Element == 0, write('.'), write(' ');
    Element == 1, write('x'), write(' ');
    Element == 2, write('o'), write(' ');
    write(Element), write(' ').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Display Matrix

%% Affiche le contenu d'une matrice
%% @param Matrix la matrice à afficher
displayMatrix(Matrix):-
    nl, displayMatrixRows(Matrix, 1),!.

%% Affiche les lignes de la matrice
%% @param Matrix la matrice à afficher
%% @param RowNb Numéro de ligne à afficher
displayMatrixRows([], _).
displayMatrixRows([Row|Rest], RowNb):-
    displayMatrixRow(Row, 1), nl,
    NextRowNb is RowNb+1,
    displayMatrixRows(Rest, NextRowNb).

%% Affiche une ligne de la matrice
%% @param Row La ligne à afficher
%% @param ColNb Numéro de colonne à afficher
displayMatrixRow(_, 10).
displayMatrixRow([Element|Rest], ColNb):-
    write(Element), write(' '),
    NextColNb is ColNb+1,
    displayMatrixRow(Rest, NextColNb).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Utils

%% Affiche des espaces
%% @param NbSpace Nombre d'espace à afficher
displaySpaces(0).
displaySpaces(NbSpace):-
    write(' '),
    NbSpaceLeft is NbSpace-1,
    displaySpaces(NbSpaceLeft).