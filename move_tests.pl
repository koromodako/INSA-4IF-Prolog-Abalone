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
    print('Initial board'),
    display:displayBoard(Board),
    % Test : Affiche la grille de jeu
    print(' ----------- Tests -----------'),
    (
        (   print(' ------- shiftUp colonne 2'),
            move:shiftUp(Board, 2, NB1),
            display:displayBoard(NB1),
            print(' ------- shiftDown colonne 2'),
            move:shiftDown(Board, 2, NB2),
            display:displayBoard(NB2),
            print(' ------- shiftDiagTTB 0'),
            move:shiftDiagTTB(Board, 0, NB3),
            display:displayBoard(NB3),
            print(' ------- shiftDiagBTT 0'),
            move:shiftDiagBTT(Board, 0, NB4),
            display:displayBoard(NB4),
            print('SUCCESS\n'), 
            !)
        ;
        (print('...FAIL\n'))
    ),
    Result is 0.
    

