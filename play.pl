%% -----------------------------------------------------------------------------
%% Module permettant de jouer à abalone en utilisant les IA
:- module(play, []).
%% -----------------------------------------------------------------------------

:- use_module('board.pl').
:- use_module('gameOver.pl').
:- use_module('iaRandom.pl').

%% Joue le tour du joueur passé en argument
%% @param Board Plateau de jeu
%% @param Player Le joueur qui doit jouer son tour
%% @param I Variable temporaire, pour limiter le nombre de boucles (dev)
play(Board, Player, I) :-
	I =< 10,
	
	% On s'arrête si game over (todo : afficher msg...)
	not(gameOver:gameOver(Player, Board)),
	
	print('Tour du joueur '), print(Player), nl,
	iaRandom:playTurn(Board, Player),
	
	% Tour suivant en changeant de joueur
	sleep(1),
	(
		(
			Player == 1, play(Board, 2, I+1)
		)
		;
		(
			Player == 2, play(Board, 1, I+1)
		)
	),
	
	display:displayBoard(Board)
	.

%% Lance le jeu après avoir initialisé le plateau
play :-
	board:initBoard(Board),
	play(Board, 1, 1)
	.
