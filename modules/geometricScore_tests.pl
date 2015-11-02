%% -----------------------------------------------------------------------------
%% Module contenant les tests du module "geometricScore".
:- module(geometricScoreTests, [runTests/1]).
%% -----------------------------------------------------------------------------

:- use_module(geometricScore).

%% Initialisation des plateaux de test -----------------------------------------

initTestBoard(
[ 
[ 1, 1, 0, 0, 0,-1,-1,-1,-1],
[ 1, 1, 0, 0, 0, 0,-1,-1,-1],
[ 1, 1, 1, 0, 0, 0, 0,-1,-1],
[ 1, 1, 1, 0, 0, 0, 0, 2,-1],
[ 1, 1, 1, 0, 0, 0, 2, 2, 2],
[-1, 1, 0, 0, 0, 0, 2, 2, 2],
[-1,-1, 0, 0, 0, 0, 2, 2, 2],
[-1,-1,-1, 0, 0, 0, 0, 2, 2],
[-1,-1,-1,-1, 0, 0, 0, 2, 2]
]).

initTestBoard2(
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
    initTestBoard2(Board2),
    
    % Test : Calcul du score sur la position initiale du plateau, sans variation
    % aléatoire, avec une agressivité de 500, pour le joueur 1.
    % Résultat attendu : Score == 7.66.
    write('Test 1.............'),
    (
        (
            geometricScore:geometricScore(Board, 1, Score, 500, 0),
            integer(Score) =:= 8,
        
            write('SUCCESS'), nl, !
        )
        ;
        (write('...FAIL'), nl)
    ),
    
    % Test : Calcul du score sur la position initiale du plateau, sans variation
    % aléatoire, avec une agressivité de 500, pour le joueur 2.
    % Résultat attendu : Score == -7.66.
    write('Test 2.............'),
    (
        (
            geometricScore:geometricScore(Board, 2, Score2, 500, 0),
            integer(Score2) =:= -8,
        
            write('SUCCESS'), nl, !
        )
        ;
        (write('...FAIL'), nl)
    ),
    
    % Test : Calcul du score sur une position quelconque, au cours d'une partie,
    % sans variation aléatoire, avec une agressivité de 500, pour le joueur 1.
    % Résultat attendu : Score == 1486.49.
    write('Test 3.............'),
    (
        (
            geometricScore:geometricScore(Board2, 1, Score3, 500, 0),
            integer(Score3) =:= 1486,
        
            write('SUCCESS'), nl, !
        )
        ;
        (write('...FAIL'), nl)
    ),
    
    % Test : Calcul du score sur une position quelconque, au cours d'une partie,
    % sans variation aléatoire, avec une agressivité de 500, pour le joueur 2.
    % Résultat attendu : Score == -1486.49.
    write('Test 4.............'),
    (
        (
            geometricScore:geometricScore(Board2, 2, Score4, 500, 0),
            integer(Score4) =:= -1486,
        
            write('SUCCESS'), nl, !
        )
        ;
        (write('...FAIL'), nl)
    ),
    
    % Test : Calcul du score sur la position initiale du plateau, sans variation
    % aléatoire, avec une agressivité de 1000, pour le joueur 1.
    % Résultat attendu : Score == 7.66.
    write('Test 5.............'),
    (
        (
            geometricScore:geometricScore(Board, 1, Score5, 1000, 0),
            integer(Score5) =:= 8,
        
            write('SUCCESS'), nl, !
        )
        ;
        (write('...FAIL'), nl)
    ),
    
    % Test : Calcul du score sur une position quelconque, au cours d'une partie,
    % sans variation aléatoire, avec une agressivité de 1000, pour le joueur 1.
    % Résultat attendu : Score == 2986.49.
    write('Test 6.............'),
    (
        (
            geometricScore:geometricScore(Board2, 1, Score6, 1000, 0),
            integer(Score6) =:= 2986,
        
            write('SUCCESS'), nl, !
        )
        ;
        (write('...FAIL'), nl)
    ),
    
    % Test : Calcul du score sur la position initiale du plateau, sans variation
    % aléatoire, avec une agressivité de 1500, pour le joueur 1.
    % Résultat attendu : Score == 7.66.
    write('Test 7.............'),
    (
        (
            geometricScore:geometricScore(Board, 1, Score7, 1500, 0),
            integer(Score7) =:= 8,
        
            write('SUCCESS'), nl, !
        )
        ;
        (write('...FAIL'), nl)
    ),
    
    % Test : Calcul du score sur une position quelconque, au cours d'une partie,
    % sans variation aléatoire, avec une agressivité de 1500, pour le joueur 1.
    % Résultat attendu : Score == 4486.49.
    write('Test 8.............'),
    (
        (
            geometricScore:geometricScore(Board2, 1, Score8, 1500, 0),
            integer(Score8) =:= 4486,
        
            write('SUCCESS'), nl, !
        )
        ;
        (write('...FAIL'), nl)
    ),
    
    Result = true.
    
 

    
    
    