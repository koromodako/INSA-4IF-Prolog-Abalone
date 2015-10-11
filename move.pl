%% -----------------------------------------------------------------------------
%% Module contenant les prédicats permettant de générer un nouveau board après un mouvement
:- module(move, [isDiagMovePos/4, isDiagMoveNeg/4, isVertMovePos/4, 
                        isVertMoveNeg/4, isHoriMovePos/4, isHoriMoveNeg/4,
                        moveMarbles/6]).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------
%% Les fonctions suivantes permettent de déterminer le sens du mouvement
%% -----------------------------------------------------------
isDiagMovePos(Xfrom, Yfrom, Xto, Yto) :- % Diag mov pos
  Xto = Xfrom + 1, 
  Yto = Yfrom + 1.
%
isDiagMoveNeg(Xfrom, Yfrom, Xto, Yto) :- % Diag mov neg
  Xto = Xfrom - 1, 
  Yto = Yfrom - 1.
%
isVertMovePos(Xfrom, Yfrom, Xto, Yto) :- % vert mov pos 
  Yto = Yfrom + 1.
%
isVertMoveNeg(Xfrom, Yfrom, Xto, Yto) :- % vert mov neg 
  Yto = Yfrom - 1.
%
isHoriMovePos(Xfrom, Yfrom, Xto, Yto) :- % horizontal mov pos 
  Xto = Xfrom + 1.
%
isHoriMoveNeg(Xfrom, Yfrom, Xto, Yto) :- % horizontal mov neg 
  Xto = Xfrom - 1.
%
isForbiddenMove(Xfrom, Yfrom, Xto, Yto) :- % mouvement interdit (diagonale inverse)
  Xto = Xfrom - 1,
  Yto = Yfrom + 1.  
%
isForbiddenMove(Xfrom, Yfrom, Xto, Yto) :- % mouvement interdit (diagonale inverse)
  Xto = Xfrom + 1,
  Yto = Yfrom - 1.

%% -----------------------------------------------------------
%% Les fonctions utiles à la réalisation du mouvement
%% -----------------------------------------------------------
% 
% Ajoute au début de la liste
%
append([],List,List).
append([H|T],L,[H|R]):-
    append(T,L,R).
%
% Inverse la liste
%
reverse([],[]).
reverse([X|Xs],YsX) :- reverse(Xs,Ys), append(Ys,[X],YsX).
% 
% Bouge les éléments de la ligne et réalise le 
% padding avec un 0
%
shiftRight(L,R) :-
    L=[H|T],
    append(T,[0],R).
%
% Bouge les éléments de la ligne et réalise le 
% padding avec un 0
% 
shiftLeft(L, R) :- 
    reverse(L, RL),
    RL=[H|T],
    append(T,[0],RLS),
    reverse(RLS, R).
%% -----------------------------------------------------------
%% Les fonctions suivantes permettent de réaliser le mouvement
%% -----------------------------------------------------------
moveMarbles(OldBoard, Xf; Yf, Xt, Yt, NewBoard) :- % mouvement interdit, le board n'est pas modifié
    isForbiddenMove(Xf, Yf, Xt, Yt),
    OldBoard = NewBoard.
%    
moveMarbles(OldBoard, Xf, Yfrom, Xt, Yt, NewBoard) :- % Diag move pos
    isDiagMovePos(Xf, Yf, Xt, Yt),
    .
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Diag move neg
    isDiagMoveNeg(Xf, Yf, Xt, Yt),
    .    
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Vert move pos  
    isVertMovePos(Xf, Yf, Xt, Yt),
    .    
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Vert move neg
    isVertMoveNeg(Xf, Yf, Xt, Yt),
    .    
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Hori move pos
    isHoriMovePos(Xf, Yf, Xt, Yt),
    .    
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Hori move neg
    isHoriMoveNeg(Xf, Yf, Xt, Yt),
    .