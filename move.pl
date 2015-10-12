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
                 shiftRight/2,
                 shiftUp/4,
                 shiftDown/4,
                 shiftDiagTTB/4,
                 shiftDiagBTT/4,
                 changeLineUp/8,
                 changeLineDown/8]).
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
shiftLeft(L, R) :-
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
% Factorisation du code de shiftUp et diag
%
changeLineUp(StartIndex, Limit, ColumnIndex, Row, NewRow, OldElement, NewOldElement, EndReached) :-
  (
      (EndReached=:=1, NewRow=Row) % Si on a fini on garde la meme ligne
      ;
      ( StartIndex >= Limit, 
        replace(Row, ColumnIndex, OldElement, NewRow, NewOldElement),!,
        (
          (NewOldElement=:=0, EndReached=1)
          ;
          (EndReached=0)
          )
        )
      ;
      (NewRow=Row, NewOldElement=0, EndReached=0)
  ).
%
% Factorisation du code de shiftDown
%
changeLineDown(StartIndex, Limit, ColumnIndex, Row, NewRow, OldElement, NewOldElement, EndReached) :-
  (
    (EndReached=:=1, NewRow=Row) % Si on a fini on garde la meme ligne
    ;
    ( StartIndex =< Limit, 
      replace(Row, ColumnIndex, OldElement, NewRow, NewOldElement),!,
      (
        (NewOldElement=:=0, EndReached=1)
        ;
        (EndReached=0)
      )
    )
    ;
    (NewRow=Row, NewOldElement=0, EndReached=0)
  ).

%
% Bouge les éléments de la colonne C vers le haut
% et réalise le padding avec un 0
%
shiftUp(Matrix, StartIndex, ColumnIndex, ResultMatrix) :-
    Matrix=[R1,R2,R3,R4,R5,R6,R7,R8,R9], % Decoupage du board en lignes
    EndReached=0, % Initialisation de EndReached
    changeLineUp(StartIndex, 9, ColumnIndex, R9, NR9, 0, OE9, EndReached),!,
    changeLineUp(StartIndex, 8, ColumnIndex, R8, NR8, OE9, OE8, EndReached),!,
    changeLineUp(StartIndex, 7, ColumnIndex, R7, NR7, OE8, OE7, EndReached),!,
    changeLineUp(StartIndex, 6, ColumnIndex, R6, NR6, OE7, OE6, EndReached),!,
    changeLineUp(StartIndex, 5, ColumnIndex, R5, NR5, OE6, OE5, EndReached),!,
    changeLineUp(StartIndex, 4, ColumnIndex, R4, NR4, OE5, OE4, EndReached),!,
    changeLineUp(StartIndex, 3, ColumnIndex, R3, NR3, OE4, OE3, EndReached),!,
    changeLineUp(StartIndex, 2, ColumnIndex, R2, NR2, OE3, OE2, EndReached),!,
    changeLineUp(StartIndex, 1, ColumnIndex, R1, NR1, OE2, _, EndReached),!,
    ResultMatrix=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].

%
% Bouge les éléments de la colonne C vers le bas
% et réalise le padding avec un 0
%
shiftDown(Matrix, StartIndex, ColumnIndex, ResultMatrix) :-
    Matrix=[R1,R2,R3,R4,R5,R6,R7,R8,R9],
    EndReached=0,
    changeLineDown(StartIndex, 1, ColumnIndex, R1, NR1, 0, OE1, EndReached),
    changeLineDown(StartIndex, 2, ColumnIndex, R2, NR2, OE1, OE2, EndReached),
    changeLineDown(StartIndex, 3, ColumnIndex, R3, NR3, OE2, OE3, EndReached),
    changeLineDown(StartIndex, 4, ColumnIndex, R4, NR4, OE3, OE4, EndReached),
    changeLineDown(StartIndex, 5, ColumnIndex, R5, NR5, OE4, OE5, EndReached),
    changeLineDown(StartIndex, 6, ColumnIndex, R6, NR6, OE5, OE6, EndReached),
    changeLineDown(StartIndex, 7, ColumnIndex, R7, NR7, OE6, OE7, EndReached),
    changeLineDown(StartIndex, 8, ColumnIndex, R8, NR8, OE7, OE8, EndReached),
    changeLineDown(StartIndex, 9, ColumnIndex, R9, NR9, OE8, _, EndReached),
    ResultMatrix=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].

%
% Bouge les éléments de la diagonale D
% vers le bas a droite
% /!\ Attention D varie dans [-8,8]
% Les diagonales sont numerote du coin 
% en bas a gauche au coin en haut a droite
% NB : TTB <=> Top To Bottom 
%
% On teste l'egalite avec =:=
%
shiftDiagTTB(Matrix, StartIndex, DiagNum, ResultMatrix) :-
    Matrix=[R1,R2,R3,R4,R5,R6,R7,R8,R9],
    EndReached=0,
    C0 is DiagNum,
    changeLineUp(C0, StartIndex, C0, R1, NR1, 0, OE1, EndReached),
    C1 is C0+1,
    changeLineUp(C1, StartIndex, C1, R2, NR2, OE1, OE2, EndReached),
    C2 is C1+1,
    changeLineUp(C2, StartIndex, C2, R3, NR3, OE2, OE3, EndReached),
    C3 is C2+1,
    changeLineUp(C3, StartIndex, C3, R4, NR4, OE3, OE4, EndReached),
    C4 is C3+1,
    changeLineUp(C4, StartIndex, C4, R5, NR5, OE4, OE5, EndReached),
    C5 is C4+1,
    changeLineUp(C5, StartIndex, C5, R6, NR6, OE5, OE6, EndReached),
    C6 is C5+1,
    changeLineUp(C6, StartIndex, C6, R7, NR7, OE6, OE7, EndReached),
    C7 is C6+1,
    changeLineUp(C7, StartIndex, C7, R8, NR8, OE7, OE8, EndReached),
    C8 is C7+1,
    changeLineUp(C8, StartIndex, C8, R9, NR9, OE8, _, EndReached),
    ResultMatrix=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].
    
%
% Bouge les éléments de la diagonale D
% vers le haut à gauche
% /!\ Attention D varie dans [-8,8]
% Les diagonales sont numerote du coin 
% en bas a gauche au coin en haut a droite
% NB : BTT <=> Bottom To Top 
%
shiftDiagBTT(Matrix, StartIndex, DiagNum, ResultMatrix) :-
    Matrix=[R1,R2,R3,R4,R5,R6,R7,R8,R9],
    EndReached=0,
    C0 is 8+DiagNum,
    changeLineDown(C0, StartIndex, C0, R9, NR9, 0, OE9, EndReached),
    C1 is C0-1,
    changeLineDown(C1, StartIndex, C1, R8, NR8, OE9, OE8, EndReached),
    C2 is C1-1,
    changeLineDown(C2, StartIndex, C2, R7, NR7, OE8, OE7, EndReached),
    C3 is C2-1,
    changeLineDown(C3, StartIndex, C3, R6, NR6, OE7, OE6, EndReached),
    C4 is C3-1,
    changeLineDown(C4, StartIndex, C4, R5, NR5, OE6, OE5, EndReached),
    C5 is C4-1,
    changeLineDown(C5, StartIndex, C5, R4, NR4, OE5, OE4, EndReached),
    C6 is C5-1,
    changeLineDown(C6, StartIndex, C6, R3, NR3, OE4, OE3, EndReached),
    C7 is C6-1,
    changeLineDown(C7, StartIndex, C7, R2, NR2, OE3, OE2, EndReached),
    C8 is C7-1,
    changeLineDown(C8, StartIndex, C8, R1, NR1, OE2, _, EndReached),
    ResultMatrix=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].

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
    S is Yf-1,
    shiftDiagTTB(OldBoard, S, D, TmpBoard),
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
    S is Yf-1,
    shiftDiagBTT(OldBoard, S, D, TmpBoard),
    rebuildEmptyCells(TmpBoard, NewBoard).
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Vert move pos  
    isVertMoveDown(Xf, Yf, Xt, Yt),
    C is Xf-1,
    S is Yf-1,
    shiftDown(OldBoard, S, C, TmpBoard),
    rebuildEmptyCells(TmpBoard, NewBoard).
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Vert move neg
    isVertMoveUp(Xf, Yf, Xt, Yt),
    C is Xf-1,
    S is Yf-1,
    shiftUp(OldBoard, S, C, TmpBoard),
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
 