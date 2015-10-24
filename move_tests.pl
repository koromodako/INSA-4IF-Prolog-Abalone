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

initTestBoard2(
    [
        [-1,-1,-1,-1,-1,-1,-1,-1,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1,-1,-1,-1,-1,-1,-1,-1,-1]
    ]).

initTestBoard3(
    [
        [ 1, 0, 0, 0,0,-1,-1,-1,-1],
        [ 0, 1, 0, 0,0, 0,-1,-1,-1],
        [ 0, 0, 0, 0,0, 1, 1,-1,-1],
        [ 0, 0, 0, 0,2, 1, 1, 0,-1],
        [ 0, 0, 0, 2,2, 2, 0, 0, 0],
        [-1, 0, 1, 1,2, 0, 0, 0, 0],
        [-1,-1, 1, 1,0, 0, 0, 0, 0],
        [-1,-1,-1, 0,0, 0, 0, 1, 0],
        [-1,-1,-1,-1,0, 0, 0, 0, 1]
    ]).
    
initTestBoardError1(
    [
        [ 1, 1, 0, 0,0,-1,-1,-1,-1],
        [ 0, 1, 0, 0,0, 0,-1,-1,-1],
        [ 0, 0, 1, 1,1, 0, 0,-1,-1],
        [ 1, 1, 1, 0,2, 0, 0, 2,-1],
        [ 1, 1, 1, 1,2, 2, 2, 2, 2],
        [-1, 1, 0, 0,0, 2, 2, 2, 2],
        [-1,-1, 0, 0,0, 0, 0, 2, 2],
        [-1,-1,-1, 0,0, 0, 0, 0, 0],
        [-1,-1,-1,-1,0 ,0, 0, 2, 0]
    ]).
    
initTestBoardError1Expected(
    [
        [ 1, 1, 0, 0,0,-1,-1,-1,-1],
        [ 0, 1, 0, 0,0, 0,-1,-1,-1],
        [ 0, 0, 1, 1,1, 0, 0,-1,-1],
        [ 1, 1, 1, 0,2, 0, 0, 2,-1],
        [ 1, 1, 1, 0,2, 2, 2, 2, 2],
        [-1, 1, 0, 0,1, 2, 2, 2, 2],
        [-1,-1, 0, 0,0, 0, 0, 2, 2],
        [-1,-1,-1, 0,0, 0, 0, 0, 0],
        [-1,-1,-1,-1,0, 0, 0, 2, 0]
    ]).

%% Lancement des tests ---------------------------------------------------------

runTests(Result) :-
    initTestBoard(Board),
    initTestBoard2(Board2),
    initTestBoard3(Board3),
    initTestBoardError1(BoardError1),
    initTestBoardError1Expected(BoardError1Expected),
    print('Initial board'),nl,
    %display:displayMatrix(Board),
    %display:displayMatrix(Board2),
    %display:displayMatrix(Board3),
    % Test : Affiche la grille de jeu
    print(' ----------- Tests Unitaires -----------'),nl,
    (
        (   
            print(' ------- Condition de depart respectee ---------'), nl,nl,
            
            print(' ------- shiftUp'),nl,
            move:shiftUp(Board, 7, 2, NB1),
            display:displayMatrix(NB1),nl,
            print(' ------- shiftDown'),nl,
            move:shiftDown(Board, 2, 2, NB2),
            display:displayMatrix(NB2),nl,
            print(' ------- shiftDiagTTB'),nl,
            move:shiftDiagTTB(Board, 1, 0, NB3),
            display:displayMatrix(NB3),nl,
            print(' ------- shiftDiagBTT'),nl,
            move:shiftDiagBTT(Board, 1, 0, NB4),
            display:displayMatrix(NB4),nl,
            print(' ------- shiftRight'),nl,
            move:shiftRight(Board, 2, 2, NB5),
            display:displayMatrix(NB5),nl,
            print(' ------- shiftLeft'),nl,
            move:shiftLeft(Board, 2, 2, NB6),
            display:displayMatrix(NB6),nl,
            
            print(' ------- Condition de fin respectee ---------'), nl,nl,
            
            print(' ------- shiftUp'),nl,
            move:shiftUp(Board2, 4, 3, NB21),
            display:displayMatrix(NB21),nl,
            print(' ------- shiftDown'),nl,
            move:shiftDown(Board2, 5, 3, NB22),
            display:displayMatrix(NB22),nl,
            print(' ------- shiftDiagTTB'),nl,
            move:shiftDiagTTB(Board2, 3, 0, NB23),
            display:displayMatrix(NB23),nl,
            print(' ------- shiftDiagBTT'),nl,
            move:shiftDiagBTT(Board2, 5, 0, NB24),
            display:displayMatrix(NB24),nl,
            print(' ------- shiftRight'),nl,
            move:shiftRight(Board2, 4, 4, NB25),
            display:displayMatrix(NB25),nl,
            print(' ------- shiftLeft'),nl,
            move:shiftLeft(Board2, 5, 4, NB26),
            display:displayMatrix(NB26),nl,

            print(' ------- moveMarbles ---------'), nl,
            print(' ------- Up & Down ---------'), nl,
            print(' ------- shiftUp'),nl,
            move:moveMarbles(Board3, 7, 4, 7, 3, NB31),
            display:displayBoard(Board3),
            display:displayBoard(NB31),nl,
            print(' ------- shiftDown'),nl,
            move:moveMarbles(Board3, 3, 6, 3, 7, NB32),
            display:displayBoard(Board3),
            display:displayBoard(NB32),nl,
            print(' ------- Left & Right ---------'), nl,
            print(' ------- shiftLeft'),nl,
            move:moveMarbles(Board3, 4, 7, 3, 7, NB33),
            display:displayBoard(Board3),
            display:displayBoard(NB33),nl,
            print(' ------- shiftRight'),nl,
            move:moveMarbles(Board3, 6, 3, 7, 3, NB34),
            display:displayBoard(Board3),
            display:displayBoard(NB34),nl,
            print(' ------- Diag BTT & TTB ---------'), nl,
            print(' ------- shiftDiagTTB on main diag'),nl,
            move:moveMarbles(Board3, 8, 8, 9, 9, NB35),
            display:displayBoard(Board3),
            display:displayBoard(NB35),nl,
            print(' ------- shiftDiagTTB below main diag'),nl,
            move:moveMarbles(Board3, 4, 5, 5, 6, NB36),
            display:displayBoard(Board3),
            display:displayBoard(NB36),nl,
            print(' ------- shiftDiagTTB above main diag'),nl,
            move:moveMarbles(Board3, 5, 4, 6, 5, NB37),
            display:displayBoard(Board3),
            display:displayBoard(NB37),nl,
            print(' ------- shiftDiagBTT on main diag'),nl,
            move:moveMarbles(Board3, 2, 2, 1, 1, NB38),
            display:displayBoard(Board3),
            display:displayBoard(NB38),nl,
            print(' ------- shiftDiagBTT below main diag'),nl,
            move:moveMarbles(Board3, 5, 6, 4, 5, NB39),
            display:displayBoard(Board3),
            display:displayBoard(NB39),nl,
            print(' ------- shiftDiagBTT above main diag'),nl,
            move:moveMarbles(Board3, 6, 5, 5, 4, NB30),
            display:displayBoard(Board3),
            display:displayBoard(NB30),nl,
            
            print(' ------- Erreurs résolues ---------'), nl,
            display:displayMatrix(BoardError1),
            display:displayBoard(BoardError1),
            move:moveMarbles(BoardError1, 4, 5, 5, 6, NewBoardError1), !,
            display:displayMatrix(NewBoardError1),
            display:displayBoard(NewBoardError1),
            display:displayBoard(BoardError1Expected),
            BoardError1Expected = NewBoardError1,

            print('SUCCESS\n'),nl,
            !)
        ;
        (print('...FAIL\n'),nl)
    ),
    Result is 0.
    

