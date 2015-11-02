%% -----------------------------------------------------------------------------
%% Module contenant les tests du module "gameOver".
:- module(gameOverTests, []).
%% -----------------------------------------------------------------------------

:- use_module(gameOver).

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
[-1,-1,-1, 0, 0, 0, 0, 0, 0],
[-1,-1,-1,-1, 0, 0, 1, 0, 0]
]).

%% Lancement des tests ---------------------------------------------------------

runTests(Result) :-
    initTestBoard(Board),
    
    % Test : Test de fin de jeu lorsque le joueur 1 possède assez de billes
    % Résultat : gameOver doit retourner faux.
    write('Test 1.............'),
    (
        (not(gameOver:gameOver(1, Board)), write('SUCCESS\n'), !)
        ;
        (write('...FAIL\n'))
    ),
    
    % Test : Test de fin de jeu lorsque le joueur 2 a perdu
    % Résultat : gameOver doit retourner vrai.
    write('Test 2.............'),
    (
        (gameOver:gameOver(2, Board), write('SUCCESS\n'), !)
        ;
        (write('...FAIL\n'))
    ),

    Result is 0.

% -------------------------------------------------------------------------------