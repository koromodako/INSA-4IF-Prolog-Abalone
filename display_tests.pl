%% -----------------------------------------------------------------------------
%% Module contenant les tests du module "display".
:- module(displayTests, []).
%% -----------------------------------------------------------------------------

:- use_module('display.pl').

%% Initialisation du plateau de test -------------------------------------------

initTestBoard(
[
[ 0, 0, 0, 0, 0,-1,-1,-1,-1],
[ 0, 1, 1, 2, 2, 2,-1,-1,-1],
[ 0, 0, 0, 1, 1, 2, 0,-1,-1],
[ 0, 0, 0, 2, 0, 1, 0, 1,-1],
[ 0, 0, 2, 2, 0, 1, 1, 1, 1],
[-1, 0, 0, 1, 0, 1, 0, 0, 0],
[-1,-1, 0, 0, 2, 0, 0, 0, 0],
[-1,-1,-1, 2, 0, 2, 0, 0, 0],
[-1,-1,-1,-1, 0, 0, 1, 0, 0]
]).

%% Lancement des tests ---------------------------------------------------------

runTests(Result) :-
    initTestBoard(Board),
    
    % Test : Affiche la grille de jeu
    print('Test 1.............'),
    (
        (display:displayBoard(Board), print('SUCCESS\n'), !)
        ;
        (print('...FAIL\n'))
    ),

    Result is 0.
    
