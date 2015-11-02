%% -----------------------------------------------------------------------------
%% Module contenant les tests du module "movable".
:- module(movableTests, []).
%% -----------------------------------------------------------------------------

:- use_module(movable).

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

    % Test : Essaye de pousser 3 boules adverses dans le vide, avec 2 boules
    % Résultat attendu : faux.
    write('Test 1.............'),
    (
        (not(movable:isMovable(Board, 2, 2, 2, 3)), write('SUCCESS'),nl, !)
        ;
        (write('...FAIL'),nl)
    ),

    % Test : Essaye de pousser 2 boules adverses sur une case vide, avec 3 boules
    % Résultat attendu : vrai.
    write('Test 2.............'),
    (
        (movable:isMovable(Board, 2, 6, 2, 5), write('SUCCESS'),nl, !)
        ;
        (write('...FAIL'),nl)
    ),

    % Test : Essaye de pousser 1 boule adverse sur une case vide, avec 2 boules
    % Résultat attendu : vrai.
    write('Test 3.............'),
    (
        (movable:isMovable(Board, 3, 4, 3, 5), write('SUCCESS'),nl, !)
        ;
        (write('...FAIL'),nl)
    ),

    % Test : Essaye de pousser 1 boule dans le vide (suicide)
    % Résultat attendu : faux.
    write('Test 4.............'),
    (
        (not(movable:isMovable(Board, 4, 8, 3, 8)), write('SUCCESS'),nl, !)
        ;
        (write('...FAIL'),nl)
    ),

    % Test : Essaye de pousser des boules dans le mauvais sens (mauvaise diag)
    % Résultat attendu : faux.
    write('Test 5.............'),
    (
        (not(movable:isMovable(Board, 4, 8, 5, 7)), write('SUCCESS'),nl, !)
        ;
        (write('...FAIL'),nl)
    ),

    % Test : Essaye de pousser 2 boules vers une case vide
    % Résultat attendu : vrai.
    write('Test 6.............'),
    (
        (movable:isMovable(Board, 5, 3, 5, 4), write('SUCCESS'),nl, !)
        ;
        (write('...FAIL'),nl)
    ),

    % Test : Essaye de pousser 1 boule adverse avec 2 boules, vers une case occupée
    % avec une de nos boules.
    % Résultat attendu : faux.
    write('Test 7.............'),
    (
        (not(movable:isMovable(Board, 5, 4, 4, 4)), write('SUCCESS'),nl, !)
        ;
        (write('...FAIL'),nl)
    ),

    % Test : Essaye de pousser 2 boules vers le vide (suicide)
    % Résultat attendu : faux.
    write('Test 8.............'),
    (
        (not(movable:isMovable(Board, 5, 8, 4, 8)), write('SUCCESS'),nl, !)
        ;
        (write('...FAIL'),nl)
    ),

    % Test : Essaye de pousser 4 boules
    % Résultat attendu : faux.
    write('Test 9.............'),
    (
        (not(movable:isMovable(Board, 5, 9, 5, 8)), write('SUCCESS'),nl, !)
        ;
        (write('...FAIL'),nl)
    ),

    % Test : Essaye de pousser 2 boules adverses avec 1 boule vers une case
    % occupée avec une de nos boules.
    % Résultat attendu : faux.
    write('Test 10............'),
    (
        (not(movable:isMovable(Board, 6, 4, 7, 5)), write('SUCCESS'),nl, !)
        ;
        (write('...FAIL'),nl)
    ),

    % Test : Essaye de pousser 2 boules adverses avec 3 boules, dans le vide
    % Résultat attendu : vrai.
    write('Test 11............'),
    (
        (movable:isMovable(Board, 6, 6, 5, 6), write('SUCCESS'),nl, !)
        ;
        (write('...FAIL'),nl)
    ),

    % Test : Essaye de pousser 1 boule adverse avec 2 boules, dans le vide, en diag
    % Résultat attendu : vrai.
    write('Test 12............'),
    (
        (movable:isMovable(Board, 7, 5, 8, 6), write('SUCCESS'),nl, !)
        ;
        (write('...FAIL'),nl)
    ),

    % Test : playerMovements, sur une case vide
    % Résultat : 0 déplacement
    write('Test 13............'),
    (
        (
            findall([NextLine, NextCol], movable:playerMovements(Board, 1, 0, 0, NextLine, NextCol), Movements1),
            length(Movements1, NbMovements1),
            NbMovements1 == 0,
            write('SUCCESS'), nl, !
        )
        ;
        (write('...FAIL'),nl)
    ),
    
    % Test : playerMovements, sur une bille adverse
    % Résultat : 0 déplacement
    write('Test 14............'),
    (
        (
            findall([NextLine, NextCol], movable:playerMovements(Board, 1, 2, 4, NextLine, NextCol), Movements2),
            length(Movements2, NbMovements2),
            NbMovements2 == 0,
            write('SUCCESS'), nl, !
        )
        ;
        (write('...FAIL'),nl)
    ),
    
    % Test : playerMovements, sur une bille étant bloquée dans certaines
    % directions
    % Résultat : 5 déplacements
    write('Test 15............'),
    (
        (
            findall([NextLine, NextCol], movable:playerMovements(Board, 1, 2, 2, NextLine, NextCol), Movements3),
            length(Movements3, NbMovements3),
            NbMovements3 == 5,
            write('SUCCESS'), nl, !
        )
        ;
        (write('...FAIL'),nl)
    ),


    Result is 0.