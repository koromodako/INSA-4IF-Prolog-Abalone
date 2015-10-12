%% -----------------------------------------------------------------------------
%% Module contenant les tests du module "move".
:- module(moveTests, []).
%% -----------------------------------------------------------------------------

:- use_module('display.pl').
:- use_module('move.pl').

%% Initialisation du plateau de test -------------------------------------------

initTestBoard(
    [
        [11,12,13,14,15,16,17,18,19],
        [21,22,23,24,25,26,27,28,29],
        [31,32,33,34,35,36,37,38,39],
        [41,42,43,44,45,46,47,48,49],
        [51,52,53,54,55,56,57,58,59],
        [61,62,63,64,65,66,67,68,69],
        [71,72,73,74,75,76,77,78,79],
        [81,82,83,84,85,86,87,88,89],
        [91,92,93,94,95,96,97,98,99]   
    ]).

%% Lancement des tests ---------------------------------------------------------

runTests(Result) :-
    initTestBoard(Board),
    print('Initial board'),nl,
    display:displayMatrix(Board),
    % Test : Affiche la grille de jeu
    print(' ----------- Tests -----------'),nl,
    (
        (   print(' ------- shiftUp colonne 2'),nl,nl,
            move:shiftUp(Board, 7, 2, NB1),
            %print(NB1),nl,nl,
            display:displayBoard(NB1),
            print(' ------- shiftDown colonne 2'),nl,nl,
            move:shiftDown(Board, 2, 2, NB2),
            %print(NB2),nl,nl,
            display:displayBoard(NB2),
            print(' ------- shiftDiagTTB 0-1'),nl,nl,
            move:shiftDiagTTB(Board, 1, 0, NB3),
            %print(NB3),nl,nl,
            display:displayBoard(NB3),
            print(' ------- shiftDiagBTT 0-1'),nl,nl,
            move:shiftDiagBTT(Board, 1, 0, NB4),
            %print(NB4),nl,nl,
            display:displayBoard(NB4),
            print('SUCCESS\n'),nl,
            !)
        ;
        (print('...FAIL\n'),nl)
    ),
    Result is 0.
    

