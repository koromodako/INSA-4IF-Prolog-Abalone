%% -----------------------------------------------------------------------------
%% Module contenant les prédicats permettant de générer un nouveau board après un mouvement
:- module(move, [isDiagMovePos/4, 
                 isDiagMoveNeg/4, 
                 isVertMoveUp/4, 
                 isVertMoveDown/4, 
                 isHoriMoveRight/4, 
                 isHoriMoveLeft/4,
                 moveMarbles/6, 
                 moveLeft/2,
                 moveRight/2,
                 shiftRight/4,
                 shiftLeft/4,
                 shiftUp/4,
                 shiftDown/4,
                 shiftDiagTTB/4,
                 shiftDiagBTT/4]).
%% -----------------------------------------------------------------------------

%%  Format attendu pour Matrix : certaines coordonnees sont interdites 
%%  Y <=> {a(0),b(1),c(2),d(3),e(4),f(5),g(6),h(7),i(8)}  
%%  X <=> {1,2,3,4,5,6,7,8}
%%  Les mouvements autorisés sont les mouvements en ligne vers droite et gauche
%%  Les mouvements autorisés sont les mouvements en colonne vers haut et bas
%%  Les mouvements autorisés sont les mouvements selon la diagonale naturelle 
%%    de la matrice (1a <-> 9i)
%%
%%  Notation pour chaque element de la matrice rc(d), avec r : RowIndex, c : ColumnIndex, d : DiagNumber
%%
%%  [
%%    [1a(0),1b(1),1c(2),1d(3),1e(4),1f(5),1g(6),1h(7),1i(8)],
%%    [2a(-1),2b(0),2c(1),2d(2),2e(3),2f(4),2g(5),2h(6),2i(7)],
%%    [3a(-2),3b(-1),3c(0),3d(1),3e(2),3f(3),3g(4),3h(5),3i(6)],
%%    [4a(-3),4b(-2),4c(-1),4d(0),4e(1),4f(2),4g(3),4h(4),4i(5)],
%%    [5a(-4),5b(-3),5c(-2),5d(-1),5e(0),5f(1),5g(2),5h(3),5i(4)],
%%    [6a(-5),6b(-4),6c(-3),6d(-2),6e(-1),6f(0),6g(1),6h(2),6i(3)],
%%    [7a(-6),7b(-5),7c(-4),7d(-3),7e(-2),7f(-1),7g(0),7h(1),7i(2)],
%%    [8a(-7),8b(-6),8c(-5),8d(-4),8e(-3),8f(-2),8g(-1),8h(0),8i(1)],
%%    [9a(-8),9b(-7),9c(-6),9d(-5),9e(-4),9f(-3),9g(-2),9h(-1),9i(0)]
%%  ]
%%
%%  Remarque : la plupart des predicats on ete construit pour repondre au probleme 
%%              uniquement et n'accepte donc qu'une matrice 9x9 en parametre.
%%

%
% predicat de parametrage de la valeur vide pour une cellule de la matrice
%
emptyValue(X) :- X=:=(-1).

%% -----------------------------------------------------------
%% Les fonctions suivantes permettent de déterminer le sens du mouvement
%% -----------------------------------------------------------
%
% Pour tous les predicats qui suivent Xfrom et Yfrom sont les coordonnees du point de depart
% et Xto Yto sont les coordonnees du point d'arrivee.
%
% @brief Ce predicat retourne vrai si les coordonnees correspondent a
%         un mouvement en diagonale vers le bas (1a -> 9i)
%
isDiagMoveTTB(Xfrom, Yfrom, Xto, Yto) :- % Diag mov pos
  Xto =:= Xfrom + 1, 
  Yto =:= Yfrom + 1.
%
% @brief Ce predicat retourne vrai si les coordonnees correspondent a
%         un mouvement en diagonale vers le haut (9i -> 1a)
%
isDiagMoveBTT(Xfrom, Yfrom, Xto, Yto) :- % Diag mov neg
  Xto =:= Xfrom - 1, 
  Yto =:= Yfrom - 1.
%
% @brief Ce predicat retourne vrai si les coordonnees correspondent a
%         un mouvement en colonne vers le bas
%
isVertMoveDown(_, Yfrom, _, Yto) :- % vert mov pos 
  Yto =:= Yfrom + 1.
%
% @brief Ce predicat retourne vrai si les coordonnees correspondent a
%         un mouvement en colonne vers le haut
%
isVertMoveUp(_, Yfrom, _, Yto) :- % vert mov neg 
  Yto =:= Yfrom - 1.
%
% @brief Ce predicat retourne vrai si les coordonnees correspondent a
%         un mouvement en ligne vers la gauche
%
isHoriMoveRight(Xfrom, _, Xto, _) :- % horizontal mov pos 
  Xto =:= Xfrom + 1.
%
% @brief Ce predicat retourne vrai si les coordonnees correspondent a
%         un mouvement en ligne vers la droite
%
isHoriMoveLeft(Xfrom, _, Xto, _) :- % horizontal mov neg 
  Xto =:= Xfrom - 1.
%
% @brief Ce predicat retourne vrai si le mouvement est interdit
%
isForbiddenMove(Xfrom, Yfrom, Xto, Yto) :- % mouvement interdit (diagonale inverse)
  Xto =:= Xfrom - 1,
  Yto =:= Yfrom + 1.  
%
isForbiddenMove(Xfrom, Yfrom, Xto, Yto) :- % mouvement interdit (diagonale inverse)
  Xto =:= Xfrom + 1,
  Yto =:= Yfrom - 1.

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
% Déplace les éléments de toute d'une liste vers la gauche
% et réalise le padding avec un 0
%
moveLeft(L, R) :-
    L=[_|T],
    append(T,[0],R).
%
% Déplace les éléments de toute d'une liste vers la droite
% et réalise le padding avec un 0
% 
moveRight(L, R) :- 
    reverse(L, RL),
    moveLeft(RL, RLS),
    reverse(RLS, R).

%
% Fonction de deplacement d'un element dans la ligne en fonction du contexte de deplacement (cf. shiftInRowRight)
%
moveRightOneInRow(StartIndex, ColumnIndex, Row, NewRow, OldElement, NewOldElement, EndReached, NewEndReached) :-
  ( 
    (EndReached=:=1, NewRow=Row, NewEndReached=1) % Si la fin a été atteinte dans un appel précédent, on conserve la ligne et on place le nouveau flag de fin
    ;
    (
      ColumnIndex >= StartIndex, % Sinon si l'index de la colonne courante est supérieur à l'index de départ on doit effectuer le changement
      replace(Row, ColumnIndex, OldElement, NewRowToValid, NewOldElement), % On remplace donc l'element courant par celui passe en parametre
      (  % Si l'element renvoye correspond a une cellule hors jeu, on replace la cellule hors jeu a sa place
        (emptyValue(NewOldElement), replace(NewRowToValid, ColumnIndex, NewOldElement, NewRow, _), NewEndReached=1)
        ;
        (NewOldElement=:=0, NewEndReached=1, NewRow=NewRowToValid) % Sinon si l'element enleve correspond a une cellule vide, on leve le flag de fin de ligne
        ;
        (NewEndReached=0, NewRow=NewRowToValid) % Sinon on baisse le flag de fin de ligne
      )
    )
    ;
    (NewRow=Row, NewOldElement=0, NewEndReached=0) % Sinon on renvoie la ligne non modifiee
  ).
%
% Fonction de deplacement d'un element dans la ligne en fonction du contexte de deplacement (cf. shiftInRowLeft)
%
moveLeftOneInRow(StartIndex, ColumnIndex, Row, NewRow, OldElement, NewOldElement, EndReached, NewEndReached) :-
  ( 
    (EndReached=:=1, NewRow=Row, NewEndReached=1)
    ;
    (
      ColumnIndex =< StartIndex,
      replace(Row, ColumnIndex, OldElement, NewRowToValid, NewOldElement),
      (
        (emptyValue(NewOldElement), replace(NewRowToValid, ColumnIndex, NewOldElement, NewRow, _), NewEndReached=1)
        ;
        (NewOldElement=:=0, NewEndReached=1, NewRow=NewRowToValid)
        ;
        (NewEndReached=0, NewRow=NewRowToValid)
      )
    )
    ;
    (NewRow=Row, NewOldElement=0, NewEndReached=0)
  ).

%
% Fait la même chose que moveRightOneInRow, sauf que le deplacement est realise vers la gauche
% pour les details voir moveRightOneInRow
%
shiftInRowLeft(Row, NewRow, StartIndex) :-
  C0 = 8, EndReached = 0,!,
  moveLeftOneInRow(StartIndex, C0, Row, NR0, 0, OE0, EndReached, NER0),!,
  C1 is C0-1,
  moveLeftOneInRow(StartIndex, C1, NR0, NR1, OE0, OE1, NER0, NER1),!,
  C2 is C1-1,
  moveLeftOneInRow(StartIndex, C2, NR1, NR2, OE1, OE2, NER1, NER2),!,
  C3 is C2-1,
  moveLeftOneInRow(StartIndex, C3, NR2, NR3, OE2, OE3, NER2, NER3),!,
  C4 is C3-1,
  moveLeftOneInRow(StartIndex, C4, NR3, NR4, OE3, OE4, NER3, NER4),!,
  C5 is C4-1,
  moveLeftOneInRow(StartIndex, C5, NR4, NR5, OE4, OE5, NER4, NER5),!,
  C6 is C5-1,
  moveLeftOneInRow(StartIndex, C6, NR5, NR6, OE5, OE6, NER5, NER6),!,
  C7 is C6-1,
  moveLeftOneInRow(StartIndex, C7, NR6, NR7, OE6, OE7, NER6, NER7),!,
  C8 is C7-1,
  moveLeftOneInRow(StartIndex, C8, NR7, NewRow, OE7, _, NER7, _).

%
% Déplace les éléments à partir de StartIndex (basé 0) vers la droite jusqu'a trouver une cellule vide (0) ou une cellule hors jeu (-1) 
%
shiftInRowRight(Row, NewRow, StartIndex) :-
  C0 = 0, EndReached = 0,!,
  moveRightOneInRow(StartIndex, C0, Row, NR0, 0, OE0, EndReached, NER0),!,
  C1 is C0+1,
  moveRightOneInRow(StartIndex, C1, NR0, NR1, OE0, OE1, NER0, NER1),!,
  C2 is C1+1,
  moveRightOneInRow(StartIndex, C2, NR1, NR2, OE1, OE2, NER1, NER2),!,
  C3 is C2+1,
  moveRightOneInRow(StartIndex, C3, NR2, NR3, OE2, OE3, NER2, NER3),!,
  C4 is C3+1,
  moveRightOneInRow(StartIndex, C4, NR3, NR4, OE3, OE4, NER3, NER4),!,
  C5 is C4+1,
  moveRightOneInRow(StartIndex, C5, NR4, NR5, OE4, OE5, NER4, NER5),!,
  C6 is C5+1,
  moveRightOneInRow(StartIndex, C6, NR5, NR6, OE5, OE6, NER5, NER6),!,
  C7 is C6+1,
  moveRightOneInRow(StartIndex, C7, NR6, NR7, OE6, OE7, NER6, NER7),!,
  C8 is C7+1,
  moveRightOneInRow(StartIndex, C8, NR7, NewRow, OE7, _, NER7, _).

%
% Déplace les elements de la ligne RowIndex (base 0) vers la gauche a partir de StartIndex (base 0) 
%
shiftLeft(Matrix, StartIndex, RowIndex, ResultMatrix) :-
  Matrix=[R1,R2,R3,R4,R5,R6,R7,R8,R9],!, % Decoupage du board en lignes
  ((RowIndex=:=0, shiftInRowLeft(R1, NR1, StartIndex));(NR1=R1)),!,
  ((RowIndex=:=1, shiftInRowLeft(R2, NR2, StartIndex));(NR2=R2)),!,
  ((RowIndex=:=2, shiftInRowLeft(R3, NR3, StartIndex));(NR3=R3)),!,
  ((RowIndex=:=3, shiftInRowLeft(R4, NR4, StartIndex));(NR4=R4)),!,
  ((RowIndex=:=4, shiftInRowLeft(R5, NR5, StartIndex));(NR5=R5)),!,
  ((RowIndex=:=5, shiftInRowLeft(R6, NR6, StartIndex));(NR6=R6)),!,
  ((RowIndex=:=6, shiftInRowLeft(R7, NR7, StartIndex));(NR7=R7)),!,
  ((RowIndex=:=7, shiftInRowLeft(R8, NR8, StartIndex));(NR8=R8)),!,
  ((RowIndex=:=8, shiftInRowLeft(R9, NR9, StartIndex));(NR9=R9)),!,
  ResultMatrix=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].
%
% Déplace les elements de la ligne RowIndex (base 0) vers la droite a partir de StartIndex (base 0)
%
shiftRight(Matrix, StartIndex, RowIndex, ResultMatrix) :-
  Matrix=[R1,R2,R3,R4,R5,R6,R7,R8,R9],!, % Decoupage du board en lignes
  ((RowIndex=:=0, shiftInRowRight(R1, NR1, StartIndex));(NR1=R1)),!,
  ((RowIndex=:=1, shiftInRowRight(R2, NR2, StartIndex));(NR2=R2)),!,
  ((RowIndex=:=2, shiftInRowRight(R3, NR3, StartIndex));(NR3=R3)),!,
  ((RowIndex=:=3, shiftInRowRight(R4, NR4, StartIndex));(NR4=R4)),!,
  ((RowIndex=:=4, shiftInRowRight(R5, NR5, StartIndex));(NR5=R5)),!,
  ((RowIndex=:=5, shiftInRowRight(R6, NR6, StartIndex));(NR6=R6)),!,
  ((RowIndex=:=6, shiftInRowRight(R7, NR7, StartIndex));(NR7=R7)),!,
  ((RowIndex=:=7, shiftInRowRight(R8, NR8, StartIndex));(NR8=R8)),!,
  ((RowIndex=:=8, shiftInRowRight(R9, NR9, StartIndex));(NR9=R9)),!,
  ResultMatrix=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].

%
% Deplace vers le haut un element d'une ligne appartenant a la colonne indiquee par ColumnIndex
%
changeLineUp(StartIndex, Limit, ColumnIndex, Row, NewRow, OldElement, NewOldElement, EndReached, NewEndReached) :-
  (
      (EndReached=:=1, NewRow=Row, NewEndReached=1) % Si on a fini on garde la meme ligne
      ;
      ( StartIndex >= Limit, 
        replace(Row, ColumnIndex, OldElement, NewRowToValid, NewOldElement),!,
        (
          % Si l'element enleve n'est pas instancie alors cela signifie que la ligne Row n'est pas affectee par le mouvement
          (var(NewOldElement), NewOldElement=0, NewEndReached=0, NewRow=NewRowToValid) % Cas ou replace n'a pas change la ligne
          ;
          (emptyValue(NewOldElement), replace(NewRowToValid, ColumnIndex, NewOldElement, NewRow, _), NewEndReached=1)
          ;
          (NewOldElement=:=0, NewEndReached=1, NewRow=NewRowToValid)
          ;
          (NewEndReached=0, NewRow=NewRowToValid)
          )
        )
      ;
      (NewRow=Row, NewOldElement=0, NewEndReached=0)
  ).
%
% Deplace vers le bas un element d'une ligne appartenant a la colonne indiquee par ColumnIndex
%
changeLineDown(StartIndex, Limit, ColumnIndex, Row, NewRow, OldElement, NewOldElement, EndReached, NewEndReached) :-
  (
    (EndReached=:=1, NewRow=Row, NewEndReached=1) % Si on a fini on garde la meme ligne
    ;
    ( StartIndex =< Limit, 
      replace(Row, ColumnIndex, OldElement, NewRowToValid, NewOldElement),!,
      (
        (var(NewOldElement), NewOldElement=0, NewEndReached=0, NewRow=NewRowToValid) % Cas ou replace n'a pas change la ligne
        ;
        (emptyValue(NewOldElement), replace(NewRowToValid, ColumnIndex, NewOldElement, NewRow, _), NewEndReached=1)
        ;
        (NewOldElement=:=0, NewEndReached=1, NewRow=NewRowToValid)
        ;
        (NewEndReached=0, NewRow=NewRowToValid)
      )
    )
    ;
    (NewRow=Row, NewOldElement=0, NewEndReached=0)
  ).

%
% Deplace vers le haut tous les elements de la colonne ColumnIndex (base 0) a partir de StartIndex (base 0)
%
shiftUp(Matrix, StartIndex, ColumnIndex, ResultMatrix) :-
    Matrix=[R1,R2,R3,R4,R5,R6,R7,R8,R9], % Decoupage du board en lignes
    EndReached=0, % Initialisation de EndReached
    changeLineUp(StartIndex, 8, ColumnIndex, R9, NR9, 0, OE9, EndReached, NER9),!,
    changeLineUp(StartIndex, 7, ColumnIndex, R8, NR8, OE9, OE8, NER9, NER8),!,
    changeLineUp(StartIndex, 6, ColumnIndex, R7, NR7, OE8, OE7, NER8, NER7),!,
    changeLineUp(StartIndex, 5, ColumnIndex, R6, NR6, OE7, OE6, NER7, NER6),!,
    changeLineUp(StartIndex, 4, ColumnIndex, R5, NR5, OE6, OE5, NER6, NER5),!,
    changeLineUp(StartIndex, 3, ColumnIndex, R4, NR4, OE5, OE4, NER5, NER4),!,
    changeLineUp(StartIndex, 2, ColumnIndex, R3, NR3, OE4, OE3, NER4, NER3),!,
    changeLineUp(StartIndex, 1, ColumnIndex, R2, NR2, OE3, OE2, NER3, NER2),!,
    changeLineUp(StartIndex, 0, ColumnIndex, R1, NR1, OE2, _, NER2, _),!,
    ResultMatrix=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].

%
% Deplace vers le haut tous les elements de la colonne ColumnIndex (base 0) a partir de StartIndex (base 0)
%
shiftDown(Matrix, StartIndex, ColumnIndex, ResultMatrix) :-
    Matrix=[R1,R2,R3,R4,R5,R6,R7,R8,R9],
    EndReached=0,
    changeLineDown(StartIndex, 0, ColumnIndex, R1, NR1, 0, OE1, EndReached, NER1),!,
    changeLineDown(StartIndex, 1, ColumnIndex, R2, NR2, OE1, OE2, NER1, NER2),!,
    changeLineDown(StartIndex, 2, ColumnIndex, R3, NR3, OE2, OE3, NER2, NER3),!,
    changeLineDown(StartIndex, 3, ColumnIndex, R4, NR4, OE3, OE4, NER3, NER4),!,
    changeLineDown(StartIndex, 4, ColumnIndex, R5, NR5, OE4, OE5, NER4, NER5),!,
    changeLineDown(StartIndex, 5, ColumnIndex, R6, NR6, OE5, OE6, NER5, NER6),!,
    changeLineDown(StartIndex, 6, ColumnIndex, R7, NR7, OE6, OE7, NER6, NER7),!,
    changeLineDown(StartIndex, 7, ColumnIndex, R8, NR8, OE7, OE8, NER7, NER8),!,
    changeLineDown(StartIndex, 8, ColumnIndex, R9, NR9, OE8, _, NER8, _),!,
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
    changeLineUp(C0, StartIndex, C0, R1, NR1, 0, OE1, EndReached, NER1),
    C1 is C0+1,
    changeLineUp(C1, StartIndex, C1, R2, NR2, OE1, OE2, NER1, NER2),
    C2 is C1+1,
    changeLineUp(C2, StartIndex, C2, R3, NR3, OE2, OE3, NER2, NER3),
    C3 is C2+1,
    changeLineUp(C3, StartIndex, C3, R4, NR4, OE3, OE4, NER3, NER4),
    C4 is C3+1,
    changeLineUp(C4, StartIndex, C4, R5, NR5, OE4, OE5, NER4, NER5),
    C5 is C4+1,
    changeLineUp(C5, StartIndex, C5, R6, NR6, OE5, OE6, NER5, NER6),
    C6 is C5+1,
    changeLineUp(C6, StartIndex, C6, R7, NR7, OE6, OE7, NER6, NER7),
    C7 is C6+1,
    changeLineUp(C7, StartIndex, C7, R8, NR8, OE7, OE8, NER7, NER8),
    C8 is C7+1,
    changeLineUp(C8, StartIndex, C8, R9, NR9, OE8, _, NER8, _),
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
    changeLineDown(C0, StartIndex, C0, R9, NR9, 0, OE9, EndReached, NER9),
    C1 is C0-1,
    changeLineDown(C1, StartIndex, C1, R8, NR8, OE9, OE8, NER9, NER8),
    C2 is C1-1,
    changeLineDown(C2, StartIndex, C2, R7, NR7, OE8, OE7, NER8, NER7),
    C3 is C2-1,
    changeLineDown(C3, StartIndex, C3, R6, NR6, OE7, OE6, NER7, NER6),
    C4 is C3-1,
    changeLineDown(C4, StartIndex, C4, R5, NR5, OE6, OE5, NER6, NER5),
    C5 is C4-1,
    changeLineDown(C5, StartIndex, C5, R4, NR4, OE5, OE4, NER5, NER4),
    C6 is C5-1,
    changeLineDown(C6, StartIndex, C6, R3, NR3, OE4, OE3, NER4, NER3),
    C7 is C6-1,
    changeLineDown(C7, StartIndex, C7, R2, NR2, OE3, OE2, NER3, NER2),
    C8 is C7-1,
    changeLineDown(C8, StartIndex, C8, R1, NR1, OE2, _, NER2, _),
    ResultMatrix=[NR1,NR2,NR3,NR4,NR5,NR6,NR7,NR8,NR9].

%% -----------------------------------------------------------
%% Les fonctions suivantes permettent de réaliser le mouvement
%% -----------------------------------------------------------
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % mouvement interdit, le board n'est pas modifié
    isForbiddenMove(Xf, Yf, Xt, Yt),
    OldBoard = NewBoard,
    !.
%    
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Diag move pos
    isDiagMovePos(Xf, Yf, Xt, Yt),
    (
        (Xf=:=Yf, DiagNum=0) % On est sur la diagonale principale
        ;
        (DiagNum is Yf-Xf) % On est sur un des deux triangles
    ),
    StartIndex is Yf-1,
    shiftDiagTTB(OldBoard, StartIndex, DiagNum, NewBoard),
    !.
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Diag move neg
    isDiagMoveNeg(Xf, Yf, Xt, Yt),
    (
        (Xf=:=Yf, DiagNum=0) % On est sur la diagonale principale
        ;
        (DiagNum is Yf-Xf) % On est sur un des deux triangles
    ),
    StartIndex is Yf-1,
    shiftDiagBTT(OldBoard, StartIndex, DiagNum, NewBoard),
    !.
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Vert move pos  
    isVertMoveDown(Xf, Yf, Xt, Yt),
    ColumnIndex is Xf-1,
    StartIndex is Yf-1,
    shiftDown(OldBoard, StartIndex, ColumnIndex, NewBoard),
    !.
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Vert move neg
    isVertMoveUp(Xf, Yf, Xt, Yt),
    ColumnIndex is Xf-1,
    StartIndex is Yf-1,
    shiftUp(OldBoard, StartIndex, ColumnIndex, NewBoard),
    !.
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Hori move pos
    isHoriMoveRight(Xf, Yf, Xt, Yt),
    RowIndex is Yf-1,
    StartIndex is Xf-1,
    shiftRight(OldBoard, StartIndex, RowIndex, NewBoard),
    !.
%
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Hori move neg
    isHoriMoveLeft(Xf, Yf, Xt, Yt),
    RowIndex is Yf-1,
    StartIndex is Xf-1,
    shiftLeft(OldBoard, StartIndex, RowIndex, NewBoard),
    !.
 