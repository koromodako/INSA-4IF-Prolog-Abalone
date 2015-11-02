%
% Inclusion des modules de tests
%
:- use_module('modules/display_tests').
:- use_module('modules/movable_tests').
:- use_module('modules/gameOver_tests').
:- use_module('modules/move_tests').

runAllTests:-
    displayTests:runTests(_),
    gameOverTests:runTests(_),
    moveTests:runTests(_),
    movableTests:runTests(_).