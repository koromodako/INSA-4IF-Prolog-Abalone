%% -----------------------------------------------------------------------------
%% Module contenant les prédicats permettant d'afficher la grille de jeu
:- module(gameOver, [gameOver/2]).
%% -----------------------------------------------------------------------------

:- use_module('board.pl').

%% 
%% Game over et son test unitaire
%% 
gameOver(Joueur, Board) :- 
	flatten(Board, InlineBoard),
	count(Joueur, InlineBoard, N), 
	N =< 8.	

% -------------------------------------------------------------------------------
