%% -----------------------------------------------------------------------------
%% Module contenant les prédicats permettant d'afficher la grille de jeu
:- module(display, [displayBoard/1, displayMatrix/1, displayBoardPosition/1,
                    displayMovements/3, displayMovement/4]).
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

%% Affiche une liste de mouvement
%% @param Line Numéro de ligne de départ
%% @param Col Numéro de colonne de départ
%% @param NextLine Numéro de ligne de destination pour un des mouvements
%% @param NextCol Numéro de colonne de destination pour un des mouvements
%% @param NextMouvement Autres mouvements
%% @param Num Numéro du mouvement actuellement affiché
displayMovements(Line, Col, [[NextLine,NextCol]|NextMouvement]):-
    displayMovements(Line, Col, [[NextLine,NextCol]|NextMouvement], 1).

displayMovements(_, _, [], _).
displayMovements(Line, Col, [[NextLine,NextCol]|NextMouvement], Num):-
    print(' '), print(Num), print(': '), displayMovement(Line, Col, NextLine, NextCol),
    NextNum is Num +1,
    displayMovements(Line, Col, NextMouvement, NextNum).

%% Affiche un mouvement
%% @param Line Numéro de ligne de départ
%% @param Col Numéro de colonne de départ
%% @param NextLine Numéro de ligne de destination 
%% @param NextCol Numéro de colonne de destination
displayMovement(Line, Col, NextLine, NextCol):-
    print(Line), displayLetter(Col),
    print(' -> '),
    print(NextLine), displayLetter(NextCol), nl.

%% Affiche en lettre un numéro de colonne
%% @param NumCol Numéro de colonne à afficher
displayLetter(1):- print('A').
displayLetter(2):- print('B').
displayLetter(3):- print('C').
displayLetter(4):- print('D').
displayLetter(5):- print('E').
displayLetter(6):- print('F').
displayLetter(7):- print('G').
displayLetter(8):- print('H').
displayLetter(9):- print('I').