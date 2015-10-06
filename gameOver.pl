% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                       Base de faits 
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(
[
[1,1,0,0,0,-1,-1,-1,-1],
[1,1,0,0,0,0,-1,-1,-1],
[1,1,1,0,0,0,0,-1,-1],
[1,1,1,0,0,0,0,2,-1],
[1,1,1,0,0,0,2,2,2],
[-1,1,0,0,0,0,2,2,2],
[-1,-1,0,0,0,0,2,2,2],
[-1,-1,-1,0,0,0,0,2,2],
[-1,-1,-1,-1,0,0,0,2,2]
]).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                       Fonctions générales 
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           Compte le nombre d'éléments dans une liste
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
count(_, [], 0) :- !. /* empty list, base case */
count(X, [X|T], N) :- /* if X is in the head of the list */
    count(X, T, N2), /* count on the tail (let this N2) */
    N is N2 + 1.     /* and N is N2 + 1  */
count(X, [Y|T], N) :- 
    X \= Y,          /* if X is not in the head */
    count(X, T, N).  /* just count the rest */

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                Game over et son test unitaire
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gameOver(Joueur, Board) :- 
	flatten(Board, InlineBoard),
	count(Joueur, InlineBoard, N), 
	N =< 8.	
% -------------------------------------------------------------- %
initGameOverUnitTest(
[
[0,0,0,0,0,-1,-1,-1,-1],
[0,0,0,0,0,0,-1,-1,-1],
[0,0,1,0,0,0,0,-1,-1],
[1,1,1,0,0,0,0,2,-1],
[1,1,1,0,0,0,2,2,2],
[-1,1,0,0,0,0,2,2,2],
[-1,-1,0,0,0,0,2,2,2],
[-1,-1,-1,0,0,0,0,2,2],
[-1,-1,-1,-1,0,0,0,2,2]
]
).
% -------------------------------------------------------------- %
gameOverUnitTest(Joueur) :- 
	initGameOverUnitTest(Board),
	gameOver(Joueur, Board).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 Move balls
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isDiagMovePos(Xfrom, Yfrom, Xto, Yto) :- % Diag mov pos
  Xto = Xfrom + 1, 
  Yto = Yfrom + 1.
isDiagMoveNeg(Xfrom, Yfrom, Xto, Yto) :- % Diag mov neg
  Xto = Xfrom - 1, 
  Yto = Yfrom - 1.
isVertMovePos(Xfrom, Yfrom, Xto, Yto) :- % vert mov pos 
  Yto = Yfrom + 1.
isVertMoveNeg(Xfrom, Yfrom, Xto, Yto) :- % vert mov neg 
  Yto = Yfrom - 1.
isHoriMovePos(Xfrom, Yfrom, Xto, Yto) :- % horizontal mov pos 
  Xto = Xfrom + 1.
isHoriMoveNeg(Xfrom, Yfrom, Xto, Yto) :- % horizontal mov neg 
  Xto = Xfrom - 1.
  
moveBalls(OldBoard, Xfrom, Yfrom, Xto, Yto, NewBoard) :- % Diag move pos or neg
	isDiagMovePos(Xfrom, Yfrom, Xto, Yto),
	.
moveBalls(OldBoard, Xfrom, Yfrom, Xto, Yto, NewBoard) :- % Diag move pos or neg
	isDiagMoveNeg(Xfrom, Yfrom, Xto, Yto),
	.	
moveBalls(OldBoard, Xfrom, Yfrom, Xto, Yto, NewBoard) :- 
	isVertMovePos(Xfrom, Yfrom, Xto, Yto),
	.	
moveBalls(OldBoard, Xfrom, Yfrom, Xto, Yto, NewBoard) :- 
	isVertMoveNeg(Xfrom, Yfrom, Xto, Yto),
	.	
moveBalls(OldBoard, Xfrom, Yfrom, Xto, Yto, NewBoard) :- 
	isHoriMovePos(Xfrom, Yfrom, Xto, Yto),
	.	
moveBalls(OldBoard, Xfrom, Yfrom, Xto, Yto, NewBoard) :- 
	isHoriMoveNeg(Xfrom, Yfrom, Xto, Yto),
	.	
% -------------------------------------------------------------- %
initMoveBallsUnitTest(
[
[1,1,0,0,0,-1,-1,-1,-1],
[1,1,0,0,0,0,-1,-1,-1],
[1,1,1,0,0,0,0,-1,-1],
[1,1,1,0,0,0,0,2,-1],
[1,1,1,0,0,0,2,2,2],
[-1,1,0,0,0,0,2,2,2],
[-1,-1,0,0,0,0,2,2,2],
[-1,-1,-1,0,0,0,0,2,2],
[-1,-1,-1,-1,0,0,0,2,2]
]
).
% -------------------------------------------------------------- %
moveBallsUnitTest(Joueur) :- 
	initMoveBallsUnitTest(Board),
	moveBalls(Board, , , , , NewBoard).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
