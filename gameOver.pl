%% -----------------------------------------------------------------------------
%% Module contenant les prédicats permettant d'afficher la grille de jeu
:- module(gameOver, [gameOver/2]).
%% -----------------------------------------------------------------------------

:- use_module(board).

%% Game over
%% Vérification de la condition d'arrêt du jeu
%% @param Board Plateau de jeu
%% @param Player Le joueur à vérifier
gameOver(Joueur, Board) :- 
    flatten(Board, InlineBoard),
    count(Joueur, InlineBoard, N),
    N =< 8.

% -------------------------------------------------------------------------------
