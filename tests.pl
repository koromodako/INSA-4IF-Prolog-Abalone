%
% Inclusion des modules de tests
%
:- use_module('modules/display_tests').
:- use_module('modules/movable_tests').
:- use_module('modules/gameOver_tests').
:- use_module('modules/move_tests').
:- use_module('modules/geometricScore_tests').

% Lancement de tous les tests unitaires
runAllTests:-
	nl,
	write('------------------------ Module : display ------------------------'),
    nl, nl,
    displayTests:runTests(_),
    
    nl,
    write('------------------------ Module : gameOver -----------------------'),
    nl, nl,
    gameOverTests:runTests(_),
    
    nl,
    write('-------------------------- Module : move -------------------------'),
    nl, nl,
    moveTests:runTests(_),
    
    nl,
    write('------------------------ Module : movable ------------------------'),
    nl, nl,
    movableTests:runTests(_),
    
    nl,
    write('--------------------- Module : geometricScore --------------------'),
    nl, nl,
    geometricScoreTests:runTests(_),
    
    nl, nl.