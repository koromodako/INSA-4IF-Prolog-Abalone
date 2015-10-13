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
	I =< 1000,
	print('Iteration : '), print(I),nl, % DEBUG
	% On s'arrête si game over (todo : afficher msg...)
	not(gameOver:gameOver(Player, Board)),
	
	print('Tour du joueur '), print(Player), nl,
	
	display:displayBoard(Board),
	
	iaRandom:playTurn(Board, Player, NewBoard),
	
	display:displayBoard(NewBoard),
	
	NextI is I+1,
	% Tour suivant en changeant de joueur
	%sleep(1),
	(
		(
			Player == 1, play(NewBoard, 2, NextI)
		)
		;
		(
			Player == 2, play(NewBoard, 1, NextI)
		)
	)
	
	
	.

%% Lance le jeu après avoir initialisé le plateau
play :-
	board:initBoard(Board),
	play(Board, 1, 1)
	.
