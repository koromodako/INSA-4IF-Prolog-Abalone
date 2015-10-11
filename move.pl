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
% Remplace le I-eme element de la liste
% index demarre a 0
%
replace([O|T], 0, X, [X|T], O).
replace([H|T], I, X, [H|R], O):- 
    I > -1, 
    NI is I-1,
    NI=I,
    O=H, 
    replace(T, NI, X, R, O), 
    !.
replace([H|T], I, X, [H|R], O):- 
    I > -1, 
    NI is I-1, 
    replace(T, NI, X, R, O), 
    !.
replace(L, _, _, L, _).
% 
% Bouge les éléments de la ligne vers la droite
% et réalise le padding avec un 0
%
shiftRight(L,R) :-
    L=[H|T],
    append(T,[0],R).
%
% Bouge les éléments de la ligne vers la gauche
% et réalise le padding avec un 0
% 
shiftLeft(L, R) :- 
    reverse(L, RL),
    RL=[H|T],
    append(T,[0],RLS),
    reverse(RLS, R).
%
% Bouge les éléments de la colonne C vers le haut
% et réalise le padding avec un 0
%
shiftUp(M, C, RM) :-
    M=[R1,R2,R3,R4,R5,R6,R7,R8,R9],
    replace(R9, C, 0, NR9, OE9),!,
    replace(R8, C, OE9, NR8, OE8),!,
    replace(R7, C, OE8, NR7, OE7),!,
    replace(R6, C, OE7, NR6, OE6),!,
    replace(R5, C, OE6, NR5, OE5),!,
    replace(R4, C, OE5, NR4, OE4),!,
    replace(R3, C, OE4, NR3, OE3),!,
    replace(R2, C, OE3, NR2, OE2),!,
    replace(R1, C, OE2, NR1, _),!,
    RM=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].

%
% Bouge les éléments de la colonne C vers le bas
% et réalise le padding avec un 0
%
shiftDown(M, C, RM) :-
    M=[R1,R2,R3,R4,R5,R6,R7,R8,R9],
    replace(R1, C, 0, NR1, OE1),!,
    replace(R2, C, OE1, NR2, OE2),!,
    replace(R3, C, OE2, NR3, OE3),!,
    replace(R4, C, OE3, NR4, OE4),!,
    replace(R5, C, OE4, NR5, OE5),!,
    replace(R6, C, OE5, NR6, OE6),!,
    replace(R7, C, OE6, NR7, OE7),!,
    replace(R8, C, OE7, NR8, OE8),!,
    replace(R9, C, OE8, NR9, _),!,
    RM=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].

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