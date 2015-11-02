%% -----------------------------------------------------------------------------
%% Module contenant les tests du module "move".
:- module(moveTests, []).
%% -----------------------------------------------------------------------------

:- use_module(display).
:- use_module(move).

%% Faits d'initialisation des plateaux -------------------------------------------

%% Plateaux de depart ~~~~~~~~~~~~~~~~

initTestBoard1(
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

%% Plateaux attendus ~~~~~~~~~~~~~~~~~

%% Expected board 1x

expectedBoard11(B) :-
    B = [
        [11,12,23,14,15,16,17,18,19],
        [21,22,33,24,25,26,27,28,29],
        [31,32,43,34,35,36,37,38,39],
        [41,42,53,44,45,46,47,48,49],
        [51,52,63,54,55,56,57,58,59],
        [61,62,73,64,65,66,67,68,69],
        [71,72,83,74,75,76,77,78,79],
        [81,82, 0,84,85,86,87,88,89],
        [91,92,93,94,95,96,97,98,99]   
    ].

expectedBoard12(B) :-
    B = [
        [11,12,13,14,15,16,17,18,19],
        [21,22,23,24,25,26,27,28,29],
        [31,32, 0,34,35,36,37,38,39],
        [41,42,33,44,45,46,47,48,49],
        [51,52,43,54,55,56,57,58,59],
        [61,62,53,64,65,66,67,68,69],
        [71,72,63,74,75,76,77,78,79],
        [81,82,73,84,85,86,87,88,89],
        [91,92,83,94,95,96,97,98,99]   
    ].

expectedBoard13(B) :- 
    B = [
        [11,12,13,14,15,16,17,18,19],
        [21, 0,23,24,25,26,27,28,29],
        [31,32,22,34,35,36,37,38,39],
        [41,42,43,33,45,46,47,48,49],
        [51,52,53,54,44,56,57,58,59],
        [61,62,63,64,65,55,67,68,69],
        [71,72,73,74,75,76,66,78,79],
        [81,82,83,84,85,86,87,77,89],
        [91,92,93,94,95,96,97,98,88]   
    ].

expectedBoard14(B) :-
    B = [
        [22,12,13,14,15,16,17,18,19],
        [21, 0,23,24,25,26,27,28,29],
        [31,32,33,34,35,36,37,38,39],
        [41,42,43,44,45,46,47,48,49],
        [51,52,53,54,55,56,57,58,59],
        [61,62,63,64,65,66,67,68,69],
        [71,72,73,74,75,76,77,78,79],
        [81,82,83,84,85,86,87,88,89],
        [91,92,93,94,95,96,97,98,99]   
    ].

expectedBoard15(B) :-
    B = [
        [11,12,13,14,15,16,17,18,19],
        [21,22,23,24,25,26,27,28,29],
        [31,32, 0,33,34,35,36,37,38],
        [41,42,43,44,45,46,47,48,49],
        [51,52,53,54,55,56,57,58,59],
        [61,62,63,64,65,66,67,68,69],
        [71,72,73,74,75,76,77,78,79],
        [81,82,83,84,85,86,87,88,89],
        [91,92,93,94,95,96,97,98,99] 
    ].

expectedBoard16(B) :-
    B = [
        [11,12,13,14,15,16,17,18,19],
        [21,22,23,24,25,26,27,28,29],
        [32,33, 0,34,35,36,37,38,39],
        [41,42,43,44,45,46,47,48,49],
        [51,52,53,54,55,56,57,58,59],
        [61,62,63,64,65,66,67,68,69],
        [71,72,73,74,75,76,77,78,79],
        [81,82,83,84,85,86,87,88,89],
        [91,92,93,94,95,96,97,98,99]   
    ].

%% Expected board 2x

expectedBoard21(B) :- 
    B = [
        [-1,-1,-1,-1,-1,-1,-1,-1,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 1, 0, 0, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 0, 1, 1, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1,-1,-1,-1,-1,-1,-1,-1,-1]   
    ].

expectedBoard22(B) :- 
    B = [
        [-1,-1,-1,-1,-1,-1,-1,-1,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 0, 1, 1, 0, 0,-1],
        [-1, 0, 0, 1, 0, 0, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1,-1,-1,-1,-1,-1,-1,-1,-1]   
    ].

expectedBoard23(B) :- 
    B = [
        [-1,-1,-1,-1,-1,-1,-1,-1,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 0, 1, 1, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 1, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1,-1,-1,-1,-1,-1,-1,-1,-1]   
    ].

expectedBoard24(B) :- 
    B = [
        [-1,-1,-1,-1,-1,-1,-1,-1,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 1, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 1, 1, 0, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1,-1,-1,-1,-1,-1,-1,-1,-1]   
    ].

expectedBoard25(B) :- 
    B = [
        [-1,-1,-1,-1,-1,-1,-1,-1,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 1, 0, 1, 1, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1,-1,-1,-1,-1,-1,-1,-1,-1]   
    ].

expectedBoard26(B) :- 
    B = [
        [-1,-1,-1,-1,-1,-1,-1,-1,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 1, 1, 1, 0, 0, 0,-1],
        [-1, 0, 0, 1, 1, 1, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1, 0, 0, 0, 0, 0, 0, 0,-1],
        [-1,-1,-1,-1,-1,-1,-1,-1,-1]   
    ].

%% Expected board 3x

expectedBoard31(B) :- 
    B = [
        [ 1, 0, 0, 0,0,-1,-1,-1,-1],
        [ 0, 1, 0, 0,0, 0,-1,-1,-1],
        [ 0, 0, 0, 0,0, 1, 1,-1,-1],
        [ 0, 0, 0, 0,2, 1, 0, 0,-1],
        [ 0, 0, 0, 2,2, 2, 0, 0, 0],
        [-1, 0, 1, 1,2, 0, 0, 0, 0],
        [-1,-1, 1, 1,0, 0, 0, 0, 0],
        [-1,-1,-1, 0,0, 0, 0, 1, 0],
        [-1,-1,-1,-1,0, 0, 0, 0, 1] 
    ].

expectedBoard32(B) :- 
    B = [
        [ 1, 0, 0, 0,0,-1,-1,-1,-1],
        [ 0, 1, 0, 0,0, 0,-1,-1,-1],
        [ 0, 0, 0, 0,0, 1, 1,-1,-1],
        [ 0, 0, 0, 0,2, 1, 1, 0,-1],
        [ 0, 0, 0, 2,2, 2, 0, 0, 0],
        [-1, 0, 0, 1,2, 0, 0, 0, 0],
        [-1,-1, 1, 1,0, 0, 0, 0, 0],
        [-1,-1,-1, 0,0, 0, 0, 1, 0],
        [-1,-1,-1,-1,0, 0, 0, 0, 1]  
    ].

expectedBoard33(B) :- 
    B = [
        [ 1, 0, 0, 0,0,-1,-1,-1,-1],
        [ 0, 1, 0, 0,0, 0,-1,-1,-1],
        [ 0, 0, 0, 0,0, 1, 1,-1,-1],
        [ 0, 0, 0, 0,2, 1, 1, 0,-1],
        [ 0, 0, 0, 2,2, 2, 0, 0, 0],
        [-1, 0, 1, 1,2, 0, 0, 0, 0],
        [-1,-1, 1, 0,0, 0, 0, 0, 0],
        [-1,-1,-1, 0,0, 0, 0, 1, 0],
        [-1,-1,-1,-1,0, 0, 0, 0, 1]  
    ].

expectedBoard34(B) :- 
    B = [
        [ 1, 0, 0, 0,0,-1,-1,-1,-1],
        [ 0, 1, 0, 0,0, 0,-1,-1,-1],
        [ 0, 0, 0, 0,0, 0, 1,-1,-1],
        [ 0, 0, 0, 0,2, 1, 1, 0,-1],
        [ 0, 0, 0, 2,2, 2, 0, 0, 0],
        [-1, 0, 1, 1,2, 0, 0, 0, 0],
        [-1,-1, 1, 1,0, 0, 0, 0, 0],
        [-1,-1,-1, 0,0, 0, 0, 1, 0],
        [-1,-1,-1,-1,0, 0, 0, 0, 1]  
    ].

expectedBoard35(B) :- 
    B = [
        [ 1, 0, 0, 0,0,-1,-1,-1,-1],
        [ 0, 1, 0, 0,0, 0,-1,-1,-1],
        [ 0, 0, 0, 0,0, 1, 1,-1,-1],
        [ 0, 0, 0, 0,2, 1, 1, 0,-1],
        [ 0, 0, 0, 2,2, 2, 0, 0, 0],
        [-1, 0, 1, 1,2, 0, 0, 0, 0],
        [-1,-1, 1, 1,0, 0, 0, 0, 0],
        [-1,-1,-1, 0,0, 0, 0, 0, 0],
        [-1,-1,-1,-1,0, 0, 0, 0, 1]  
    ].

expectedBoard36(B) :- 
    B = [
        [ 1, 0, 0, 0,0,-1,-1,-1,-1],
        [ 0, 1, 0, 0,0, 0,-1,-1,-1],
        [ 0, 0, 0, 0,0, 1, 1,-1,-1],
        [ 0, 0, 0, 0,2, 1, 1, 0,-1],
        [ 0, 0, 0, 0,2, 2, 0, 0, 0],
        [-1, 0, 1, 1,2, 0, 0, 0, 0],
        [-1,-1, 1, 1,0, 2, 0, 0, 0],
        [-1,-1,-1, 0,0, 0, 0, 1, 0],
        [-1,-1,-1,-1,0, 0, 0, 0, 1]  
    ].

expectedBoard37(B) :- 
    B = [
        [ 1, 0, 0, 0,0,-1,-1,-1,-1],
        [ 0, 1, 0, 0,0, 0,-1,-1,-1],
        [ 0, 0, 0, 0,0, 1, 1,-1,-1],
        [ 0, 0, 0, 0,0, 1, 1, 0,-1],
        [ 0, 0, 0, 2,2, 2, 0, 0, 0],
        [-1, 0, 1, 1,2, 0, 2, 0, 0],
        [-1,-1, 1, 1,0, 0, 0, 0, 0],
        [-1,-1,-1, 0,0, 0, 0, 1, 0],
        [-1,-1,-1,-1,0, 0, 0, 0, 1]  
    ].

expectedBoard38(B) :- 
    B = [
        [ 1, 0, 0, 0,0,-1,-1,-1,-1],
        [ 0, 0, 0, 0,0, 0,-1,-1,-1],
        [ 0, 0, 0, 0,0, 1, 1,-1,-1],
        [ 0, 0, 0, 0,2, 1, 1, 0,-1],
        [ 0, 0, 0, 2,2, 2, 0, 0, 0],
        [-1, 0, 1, 1,2, 0, 0, 0, 0],
        [-1,-1, 1, 1,0, 0, 0, 0, 0],
        [-1,-1,-1, 0,0, 0, 0, 1, 0],
        [-1,-1,-1,-1,0, 0, 0, 0, 1]  
    ].

expectedBoard39(B) :- 
    B = [
        [ 1, 0, 0, 0,0,-1,-1,-1,-1],
        [ 0, 1, 0, 0,0, 0,-1,-1,-1],
        [ 0, 0, 0, 0,0, 1, 1,-1,-1],
        [ 0, 0, 2, 0,2, 1, 1, 0,-1],
        [ 0, 0, 0, 2,2, 2, 0, 0, 0],
        [-1, 0, 1, 1,0, 0, 0, 0, 0],
        [-1,-1, 1, 1,0, 0, 0, 0, 0],
        [-1,-1,-1, 0,0, 0, 0, 1, 0],
        [-1,-1,-1,-1,0, 0, 0, 0, 1]  
    ].

expectedBoard30(B) :- 
    B = [
        [ 1, 0, 0, 0,0,-1,-1,-1,-1],
        [ 0, 1, 0, 0,0, 0,-1,-1,-1],
        [ 0, 0, 0, 2,0, 1, 1,-1,-1],
        [ 0, 0, 0, 0,2, 1, 1, 0,-1],
        [ 0, 0, 0, 2,2, 0, 0, 0, 0],
        [-1, 0, 1, 1,2, 0, 0, 0, 0],
        [-1,-1, 1, 1,0, 0, 0, 0, 0],
        [-1,-1,-1, 0,0, 0, 0, 1, 0],
        [-1,-1,-1,-1,0, 0, 0, 0, 1]  
    ].

%% Lancement des tests ---------------------------------------------------------

%% Table associative pour les sigles utilisés :
%%
%%  NB <=> NewBoard
%%
%%  TTB <=> Top To Bottom
%%  BTT <=> Bottom To Top

runTests(Result) :-
    initTestBoard1(Board1),
    initTestBoard2(Board2),
    initTestBoard3(Board3),
    write('Initial board'),nl,
    %display:displayMatrix(Board),
    %display:displayMatrix(Board2),
    %display:displayMatrix(Board3),
    % Test : Affiche la grille de jeu
    write(' ----------- Tests Unitaires -----------'),nl,
    (
        (   
            write(' ------- Condition de depart respectee ---------'), nl,nl,
            
            write(' ------- shiftUp'),nl,
            move:shiftUp(Board1, 7, 2, NB11),
            %display:displayMatrix(NB11),nl, %debug visuel
            expectedBoard11(NB11),
            
            write(' ------- shiftDown'),nl,
            move:shiftDown(Board1, 2, 2, NB12),
            %display:displayMatrix(NB12),nl, %debug visuel
            expectedBoard12(NB12),
            
            write(' ------- shiftDiagTTB'),nl,
            move:shiftDiagTTB(Board1, 1, 0, NB13),
            %display:displayMatrix(NB13),nl, %debug visuel
            expectedBoard13(NB13),
            
            write(' ------- shiftDiagBTT'),nl,
            move:shiftDiagBTT(Board1, 1, 0, NB14),
            %display:displayMatrix(NB14),nl, %debug visuel
            expectedBoard14(NB14),

            write(' ------- shiftRight'),nl,
            move:shiftRight(Board1, 2, 2, NB15),
            %display:displayMatrix(NB15),nl, %debug visuel
            expectedBoard15(NB15),

            write(' ------- shiftLeft'),nl,
            move:shiftLeft(Board1, 2, 2, NB16),
            %display:displayMatrix(NB16),nl, %debug visuel
            expectedBoard16(NB16),
            
            
            write(' ------- Condition de fin respectee ---------'), nl,nl,
            
            write(' ------- shiftUp'),nl,
            move:shiftUp(Board2, 4, 3, NB21),
            %display:displayMatrix(NB21),nl, %debug visuel
            expectedBoard21(NB21),

            write(' ------- shiftDown'),nl,
            move:shiftDown(Board2, 5, 3, NB22),
            %display:displayMatrix(NB22),nl, %debug visuel
            expectedBoard22(NB22),
                
            write(' ------- shiftDiagTTB'),nl,
            move:shiftDiagTTB(Board2, 3, 0, NB23),
            %display:displayMatrix(NB23),nl, %debug visuel
            expectedBoard23(NB23),
            
            write(' ------- shiftDiagBTT'),nl,
            move:shiftDiagBTT(Board2, 5, 0, NB24),
            %display:displayMatrix(NB24),nl, %debug visuel
            expectedBoard24(NB24),
            
            write(' ------- shiftRight'),nl,
            move:shiftRight(Board2, 4, 4, NB25),
            %display:displayMatrix(NB25),nl, %debug visuel
            expectedBoard25(NB25),
            
            write(' ------- shiftLeft'),nl,
            move:shiftLeft(Board2, 5, 4, NB26),
            %display:displayMatrix(NB26),nl, %debug visuel
            expectedBoard26(NB26),

            write(' ------- moveMarbles ---------'), nl,
            write(' ------- Up & Down ---------'), nl,
            write(' ------- shiftUp'),nl,
            move:moveMarbles(Board3, 7, 4, 7, 3, NB31),
            %display:displayMatrix(NB31),nl,
            %display:displayBoard(Board3),
            %display:displayBoard(NB31),nl,
            expectedBoard31(NB31),

            write(' ------- shiftDown'),nl,
            move:moveMarbles(Board3, 3, 6, 3, 7, NB32),
            %display:displayMatrix(NB32),nl,
            %display:displayBoard(Board3),
            %display:displayBoard(NB32),nl,
            expectedBoard32(NB32),

            write(' ------- Left & Right ---------'), nl,
            write(' ------- shiftLeft'),nl,
            move:moveMarbles(Board3, 4, 7, 3, 7, NB33),
            %display:displayMatrix(NB33),nl,
            %display:displayBoard(Board3),
            %display:displayBoard(NB33),nl,
            expectedBoard33(NB33),

            write(' ------- shiftRight'),nl,
            move:moveMarbles(Board3, 6, 3, 7, 3, NB34),
            %display:displayMatrix(NB34),nl,
            %display:displayBoard(Board3),
            %display:displayBoard(NB34),nl,
            expectedBoard34(NB34),

            write(' ------- Diag BTT & TTB ---------'), nl,
            write(' ------- shiftDiagTTB on main diag'),nl,
            move:moveMarbles(Board3, 8, 8, 9, 9, NB35),
            %display:displayBoard(Board3),
            %display:displayBoard(NB35),nl,
            expectedBoard35(NB35),

            write(' ------- shiftDiagTTB below main diag'),nl,
            move:moveMarbles(Board3, 4, 5, 5, 6, NB36),
            %display:displayBoard(Board3),
            %display:displayBoard(NB36),nl,
            expectedBoard36(NB36),

            write(' ------- shiftDiagTTB above main diag'),nl,
            move:moveMarbles(Board3, 5, 4, 6, 5, NB37),
            %display:displayBoard(Board3),
            %display:displayBoard(NB37),nl,
            expectedBoard37(NB37),

            write(' ------- shiftDiagBTT on main diag'),nl,
            move:moveMarbles(Board3, 2, 2, 1, 1, NB38),
            %display:displayBoard(Board3),
            %display:displayBoard(NB38),nl,
            expectedBoard38(NB38),

            write(' ------- shiftDiagBTT below main diag'),nl,
            move:moveMarbles(Board3, 5, 6, 4, 5, NB39),
            %display:displayBoard(Board3),
            %display:displayBoard(NB39),nl,
            expectedBoard39(NB39),

            write(' ------- shiftDiagBTT above main diag'),nl,
            move:moveMarbles(Board3, 6, 5, 5, 4, NB30),
            %display:displayBoard(Board3),
            %display:displayBoard(NB30),nl,
            expectedBoard30(NB30),
            
            write('SUCCESS\n'),nl,
            !)
        ;
        (write('...FAIL\n'),nl)
    ),
    Result is 0.
    

