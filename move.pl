%% -----------------------------------------------------------------------------
%% Module contenant les prédicats permettant de générer un nouveau board après un mouvement
:- module(move, [isDiagMovePos/4, 
                 isDiagMoveNeg/4, 
                 isVertMoveUp/4, 
                 isVertMoveDown/4, 
                 isHoriMoveRight/4, 
                 isHoriMoveLeft/4,
                 moveMarbles/6, 
                 shiftLeft/2, 
                 shiftUp/3,
                 shiftRight/2,
                 shiftDown/3,
                 shiftDiagTTB/3,
                 shiftDiagBTT/3]).
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
isVertMoveDown(_, Yfrom, _, Yto) :- % vert mov pos 
  Yto = Yfrom + 1.
%
isVertMoveUp(_, Yfrom, _, Yto) :- % vert mov neg 
  Yto = Yfrom - 1.
%
isHoriMoveRight(Xfrom, _, Xto, _) :- % horizontal mov pos 
  Xto = Xfrom + 1.
%
isHoriMoveLeft(Xfrom, _, Xto, _) :- % horizontal mov neg 
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
% Usage :
%     replace([a,b,c], 1, x, NewList, OldElem).
%     NewList = [a,x,c]
%     OldElem = b
% Note : si l'index est "out of bounds" renvoie la liste inchangée 
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
% Bouge les éléments de la ligne vers la gauche
% et réalise le padding avec un 0
%
shiftLeft(L,R) :-
    L=[_|T],
    append(T,[0],R).
%
% Bouge les éléments de la ligne vers la droite
% et réalise le padding avec un 0
% 
shiftRight(L, R) :- 
    reverse(L, RL),
    shiftLeft(RL, RLS),
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

%
% Bouge les éléments de la diagonale D
% vers le bas a droite
% /!\ Attention D varie dans [-8,8]
% Les diagonales sont numerote du coin 
% en bas a gauche au coin en haut a droite
% NB : TTB <=> Top To Bottom 
%
shiftDiagTTB(M, D, RM) :-
    M=[R1,R2,R3,R4,R5,R6,R7,R8,R9],
    C0 is D,
    replace(R1, C0, 0, NR1, OE1),
    C1 is C0+1,
    replace(R2, C1, OE1, NR2, OE2),
    C2 is C1+1,
    replace(R3, C2, OE2, NR3, OE3),
    C3 is C2+1,
    replace(R4, C3, OE3, NR4, OE4),
    C4 is C3+1,
    replace(R5, C4, OE4, NR5, OE5),
    C5 is C4+1,
    replace(R6, C5, OE5, NR6, OE6),
    C6 is C5+1,
    replace(R7, C6, OE6, NR7, OE7),
    C7 is C6+1,
    replace(R8, C7, OE7, NR8, OE8),
    C8 is C7+1,
    replace(R9, C8, OE8, NR9, _),
    RM=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].
    
%
% Bouge les éléments de la diagonale D
% vers le haut à gauche
% /!\ Attention D varie dans [-8,8]
% Les diagonales sont numerote du coin 
% en bas a gauche au coin en haut a droite
% NB : BTT <=> Bottom To Top 
%
shiftDiagBTT(M, D, RM) :-
    M=[R1,R2,R3,R4,R5,R6,R7,R8,R9],
    C0 is 8+D,
    replace(R9, C0, 0, NR9, OE9),
    C1 is C0-1,
    replace(R8, C1, OE9, NR8, OE8),
    C2 is C1-1,
    replace(R7, C2, OE8, NR7, OE7),
    C3 is C2-1,
    replace(R6, C3, OE7, NR6, OE6),
    C4 is C3-1,
    replace(R5, C4, OE6, NR5, OE5),
    C5 is C4-1,
    replace(R4, C5, OE5, NR4, OE4),
    C6 is C5-1,
    replace(R3, C6, OE4, NR3, OE3),
    C7 is C6-1,
    replace(R2, C7, OE3, NR2, OE2),
    C8 is C7-1,
    replace(R1, C8, OE2, NR1, _),
    RM=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].

%
% Remet des cases vides aux bons endroits
%
rebuildEmptyCells(OldBoard, NewBoard) :-
  OldBoard = NewBoard. % TODO : implementer ici

%% -----------------------------------------------------------
%% Les fonctions suivantes permettent de réaliser le mouvement
%% -----------------------------------------------------------
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % mouvement interdit, le board n'est pas modifié
    isForbiddenMove(Xf, Yf, Xt, Yt),
    OldBoard = NewBoard.
%    
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Diag move pos
    isDiagMovePos(Xf, Yf, Xt, Yt),
    (
        (Xf=Yf, D=0) % On est sur la diagonale principale
        ;
        (Xf>Yf, D is Xf-1) % On est sur une diagonale du triangle inf.
        ;
        (Xf<Yf, D is Yf-1) % On est sur une diagonale du triangle sup.
    ),
    shiftDiagTTB(OldBoard, D, TmpBoard),
    rebuildEmptyCells(TmpBoard, NewBoard).
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Diag move neg
    isDiagMoveNeg(Xf, Yf, Xt, Yt),
    (
        (Xf=Yf, D=0) % On est sur la diagonale principale
        ;
        (Xf>Yf, D is Xf-1) % On est sur une diagonale du triangle inf.
        ;
        (Xf<Yf, D is Yf-1) % On est sur une diagonale du triangle sup.
    ),
    shiftDiagBTT(OldBoard, D, TmpBoard),
    rebuildEmptyCells(TmpBoard, NewBoard).
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Vert move pos  
    isVertMoveDown(Xf, Yf, Xt, Yt),
    C is Xf-1,
    shiftDown(OldBoard, C, TmpBoard),
    rebuildEmptyCells(TmpBoard, NewBoard).
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Vert move neg
    isVertMoveUp(Xf, Yf, Xt, Yt),
    C is Xf-1,
    shiftUp(OldBoard, C, TmpBoard),
    rebuildEmptyCells(TmpBoard, NewBoard).
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Hori move pos
    isHoriMoveRight(Xf, Yf, Xt, Yt),
    %R is Yf-1,
    %shiftRight(OldBoard, R, TmpBoard),
    rebuildEmptyCells(TmpBoard, NewBoard).
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Hori move neg
    isHoriMoveLeft(Xf, Yf, Xt, Yt),
    %R is Yf-1,
    %shiftLeft(OldBoard, R, TmpBoard),
    rebuildEmptyCells(TmpBoard, NewBoard). 