%% -----------------------------------------------------------------------------
%% Module contenant les prédicats permettant de générer un nouveau board après un mouvement
:- module(move, [isDiagMovePos/4, isDiagMoveNeg/4, isVertMovePos/4, 
                        isVertMoveNeg/4, isHoriMovePos/4, isHoriMoveNeg/4,
                        moveMarbles/6]).
%% -----------------------------------------------------------------------------

%%
%% Les fonctions suivantes permettent de déterminer le sens du mouvement
%%
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
  
%%
%% Les fonctions suivantes permettent de réaliser le mouvement
%%
moveMarbles(OldBoard, Xf, Yfrom, Xt, Yt, NewBoard) :- % Diag move pos
    isDiagMovePos(Xf, Yf, Xt, Yt),
    OldBoard = NewBoard.
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Diag move neg
    isDiagMoveNeg(Xf, Yf, Xt, Yt),
    OldBoard = NewBoard.    
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Vert move pos  
    isVertMovePos(Xf, Yf, Xt, Yt),
    OldBoard = NewBoard.    
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Vert move neg
    isVertMoveNeg(Xf, Yf, Xt, Yt),
    OldBoard = NewBoard.    
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Hori move pos
    isHoriMovePos(Xf, Yf, Xt, Yt),
    OldBoard = NewBoard.    
moveMarbles(OldBoard, Xf, Yf, Xt, Yt, NewBoard) :- % Hori move neg
    isHoriMoveNeg(Xf, Yf, Xt, Yt),
    OldBoard = NewBoard.