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
                 shiftDiagBTT/4]).
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
% Bouge les éléments de la colonne C vers le haut
% et réalise le padding avec un 0
%
shiftUp(M, S, C, RM) :-
    M=[R1,R2,R3,R4,R5,R6,R7,R8,R9],
    (
      (S >= 9 , replace(R9, C, 0, NR9, OE9),!,((OE9=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR9=R9, OE9=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR8=R8) % Si on a fini on garde la meme ligne
      ;
      (S >= 8 , replace(R8, C, OE9, NR8, OE8),!,((OE8=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR8=R8, OE8=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR7=R7) % Si on a fini on garde la meme ligne
      ;
      (S >= 7 , replace(R7, C, OE8, NR7, OE7),!,((OE7=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR7=R7, OE7=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR6=R6) % Si on a fini on garde la meme ligne
      ;
      (S >= 6 , replace(R6, C, OE7, NR6, OE6),!,((OE6=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR6=R6, OE6=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR5=R5) % Si on a fini on garde la meme ligne
      ;
      (S >= 5 , replace(R5, C, OE6, NR5, OE5),!,((OE5=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR5=R5, OE5=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR4=R4) % Si on a fini on garde la meme ligne
      ;
      (S >= 4 , replace(R4, C, OE5, NR4, OE4),!,((OE4=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR4=R4, OE4=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR3=R3) % Si on a fini on garde la meme ligne
      ;
      (S >= 3 , replace(R3, C, OE4, NR3, OE3),!,((OE3=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR3=R3, OE3=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR2=R2) % Si on a fini on garde la meme ligne
      ;
      (S >= 2 , replace(R2, C, OE3, NR2, OE2),!,((OE2=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR2=R2, OE2=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR1=R1) % Si on a fini on garde la meme ligne
      ;
      (S >= 1 , replace(R1, C, OE2, NR1, _))
      ;
      (NR1=R1, EndReached=0)
    ),
    RM=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].

%
% Bouge les éléments de la colonne C vers le bas
% et réalise le padding avec un 0
%
shiftDown(M, S, C, RM) :-
    M=[R1,R2,R3,R4,R5,R6,R7,R8,R9],
    (
      (S =< 1 , replace(R1, C, 0, NR1, OE1),!,((OE1=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR1=R1, OE1=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR2=R2) % Si on a fini on garde la meme ligne
      ;
      (S =< 2 , replace(R2, C, OE1, NR2, OE2),!,((OE2=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR2=R2, OE2=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR3=R3) % Si on a fini on garde la meme ligne
      ;
      (S =< 3 , replace(R3, C, OE2, NR3, OE3),!,((OE3=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR3=R3, OE3=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR4=R4) % Si on a fini on garde la meme ligne
      ;
      (S =< 4 , replace(R4, C, OE3, NR4, OE4),!,((OE4=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR4=R4, OE4=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR5=R5) % Si on a fini on garde la meme ligne
      ;
      (S =< 5 , replace(R5, C, OE4, NR5, OE5),!,((OE5=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR5=R5, OE5=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR6=R6) % Si on a fini on garde la meme ligne
      ;
      (S =< 6 , replace(R6, C, OE5, NR6, OE6),!,((OE6=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR6=R6, OE6=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR7=R7) % Si on a fini on garde la meme ligne
      ;
      (S =< 7 , replace(R7, C, OE6, NR7, OE7),!,((OE7=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR7=R7, OE7=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR8=R8) % Si on a fini on garde la meme ligne
      ;
      (S =< 8 , replace(R8, C, OE7, NR8, OE8),!,((OE8=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR8=R8, OE8=0, EndReached=0)
    ),
    (
      (EndReached=:=1, NR9=R9) % Si on a fini on garde la meme ligne
      ;
      (S =< 9 , replace(R9, C, OE8, NR9, _))
      ;
      (NR9=R9, EndReached=0)
    ),
    RM=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].

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
shiftDiagTTB(M, S, D, RM) :-
    M=[R1,R2,R3,R4,R5,R6,R7,R8,R9],
    C0 is D,!,
    (
      ( C0 >= S,  % si compteur >= start,
        replace(R1, C0, 0, NR1, OE1),!, % On bouge
        (
          (OE1=:=0, EndReached=1) % Si on a enlevé un 0, alors on a fini
          ;
          (EndReached=0) % Sinon on a pas fini
        )
      ) 
      ;
      (NR1=R1, OE1=0, EndReached=0) % sinon garde la meme ligne et place 0 dans l'element enleve
    ),
    C1 is C0+1,!, % On incrémente
    (
      (EndReached=:=1, NR2=R2) % Si on a fini on garde la meme ligne
      ;
      (C1 >= S, replace(R2, C1, OE1, NR2, OE2),!,((OE2=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR2=R2, OE2=0, EndReached=0)
    ),
    C2 is C1+1,!,
    (
      (EndReached=:=1, NR3=R3)
      ;
      (C2 >= S, replace(R3, C2, OE2, NR3, OE3),!,((OE3=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR3=R3, OE3=0, EndReached=0)
    ),
    C3 is C2+1,!,
    (
      (EndReached=:=1, NR4=R4)
      ;
      (C3 >= S, replace(R4, C3, OE3, NR4, OE4),!,((OE4=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR4=R4, OE4=0, EndReached=0)
    ),
    C4 is C3+1,!,
    (
      (EndReached=:=1, NR5=R5)
      ;
      (C4 >= S, replace(R5, C4, OE4, NR5, OE5),!,((OE5=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR5=R5, OE5=0, EndReached=0)
    ),
    C5 is C4+1,!,
    (
      (EndReached=:=1, NR6=R6)
      ;
      (C5 >= S, replace(R6, C5, OE5, NR6, OE6),!,((OE6=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR6=R6, OE6=0, EndReached=0)
    ),
    C6 is C5+1,!,
    (
      (EndReached=:=1, NR7=R7)
      ;
      (C6 >= S, replace(R7, C6, OE6, NR7, OE7),!,((OE7=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR7=R7, OE7=0, EndReached=0)
    ),
    C7 is C6+1,!,
    (
      (EndReached=:=1, NR8=R8)
      ;
      (C7 >= S, replace(R8, C7, OE7, NR8, OE8),!,((OE8=:=0, EndReached=1);(EndReached=0)))
      ;
      (NR8=R8, OE8=0, EndReached=0)
    ),
    C8 is C7+1,!,
    (
      (EndReached=:=1, NR9=R9)
      ;
      (C8 >= S, replace(R9, C8, OE8, NR9, _))
      ;
      (NR9=R9)
    ),
    RM=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].
    
%
% Bouge les éléments de la diagonale D
% vers le haut à gauche
% /!\ Attention D varie dans [-8,8]
% Les diagonales sont numerote du coin 
% en bas a gauche au coin en haut a droite
% NB : BTT <=> Bottom To Top 
%
shiftDiagBTT(M, S, D, RM) :-
    M=[R1,R2,R3,R4,R5,R6,R7,R8,R9],
    C0 is 8+D,
    (
      ( C0 =< S, replace(R9, C0, 0, NR9, OE9),!,((OE9=:=0, EndReached=1);(EndReached=0)) )
      ;
      ( NR9=R9, OE9=0, EndReached=0 )
    ),
    C1 is C0-1,
    (
      ( EndReached=:=1, NR8=R8 )
      ;
      ( C1 =< S, replace(R8, C1, OE9, NR8, OE8),!,((OE8=:=0, EndReached=1);(EndReached=0)) )
      ;
      ( NR8=R8, OE8=0, EndReached=0 )
    ),
    C2 is C1-1,
    (
      ( EndReached=:=1, NR7=R7 )
      ;
      ( C2 =< S, replace(R7, C2, OE8, NR7, OE7),!,((OE7=:=0, EndReached=1);(EndReached=0)) )
      ;
      ( NR7=R7, OE7=0, EndReached=0 )
    ),
    C3 is C2-1,
    (
      ( EndReached=:=1, NR6=R6 )
      ;
      ( C3 =< S, replace(R6, C3, OE7, NR6, OE6),!,((OE6=:=0, EndReached=1);(EndReached=0)) )
      ;
      ( NR6=R6, OE6=0, EndReached=0 )
    ),
    C4 is C3-1,
    (
      ( EndReached=:=1, NR5=R5 )
      ;
      ( C4 =< S, replace(R5, C4, OE6, NR5, OE5),!,((OE5=:=0, EndReached=1);(EndReached=0)) )
      ;
      ( NR5=R5, OE5=0, EndReached=0 )
    ),
    C5 is C4-1,
    (
      ( EndReached=:=1, NR4=R4 )
      ;
      ( C5 =< S, replace(R4, C5, OE5, NR4, OE4),!,((OE4=:=0, EndReached=1);(EndReached=0)) )
      ;
      ( NR4=R4, OE4=0, EndReached=0 )
    ),
    C6 is C5-1,
    (
      ( EndReached=:=1, NR3=R3 )
      ;
      ( C6 =< S, replace(R3, C6, OE4, NR3, OE3),!,((OE3=:=0, EndReached=1);(EndReached=0)) )
      ;
      ( NR3=R3, OE3=0, EndReached=0 )
    ),
    C7 is C6-1,
    (
      ( EndReached=:=1, NR2=R2 )
      ;
      ( C7 =< S, replace(R2, C7, OE3, NR2, OE2),!,((OE2=:=0, EndReached=1);(EndReached=0)) )
      ;
      ( NR2=R2, OE2=0, EndReached=0 )
    ),
    C8 is C7-1,
    (
      ( EndReached=:=1, NR1=R1 )
      ;
      ( C8 =< S, replace(R1, C8, OE2, NR1, _) )
      ;
      ( NR1=R1 )
    ),
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
 