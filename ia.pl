%% -----------------------------------------------------------------------------
%% Module permettant de réaliser l'IA utilisant l'algorithme Alpha Beta
:- module(ia, [playTurn/3]).
%% -----------------------------------------------------------------------------

:- use_module(movable).
:- use_module(move).
:- use_module(geometricScoreFunction).

%% Joue le tour du joueur passé en argument.
%% @param Board Plateau de jeu
%% @param Player Le joueur qui doit jouer son tour
playTurn(Board, Player, NewBoard) :-
    alphabeta(true, Player, [0, 0, 0, 0, Board], -10000, 10000, BestMovement, _, 2),
    nth0(0, BestMovement, Line),
    nth0(1, BestMovement, Col),
    nth0(2, BestMovement, NextLine),
    nth0(3, BestMovement, NextCol),
    %nth0(4, BestMovement, NewBoard),
    moveMarbles(Board, Col, Line, NextCol, NextLine, NewBoard).

%% La profondeur max a été atteinte, on récupère donc la valeur de la solution
alphabeta(_, Player, Movement, _, _, _, NodeValue, 0) :-
    % Movements est une liste avec dans l'ordre : Line, Col, NextLine, NextCol,
    % NewBoard
    nth0(4, Movement, Board),
    geometricScoreFunction:geometricScore(Board, Player, NodeValue), !.

%% Alpha beta sur un noeud, c'est-à-dire un déplacement et une configuration à tester,
%% avec profondeur > 0    
alphabeta(MaxAction, Player, Movement, Alpha, Beta, BestMovement, NodeValue, Depth) :-
    
    % Récupération des données du mouvement à analyser
    nth0(4, Movement, Board),
    
    % Récupération des différents noeuds possibles
    findall(
       [Line, Col, NextLine, NextCol, NewBoard], 
       (
           movable:playerMovements(Board, Player, Line, Col, NextLine, NextCol), 
           move:moveMarbles(Board, Col, Line, NextCol, NextLine, NewBoard)
       ),
       NewMovements
    ), !, 
    NewDepth is Depth - 1,
    
    swapMinMax(MaxAction, NewMaxAction),
    
    % Analyse des noeuds fils du mouvement analysé
    alphabetaCheckNodes(NewMaxAction, Player, NewMovements, Alpha, Beta, BestMovement, NodeValue, NewDepth).

%% Analyse une collection de noeuds, afin d'appliquer alpha beta sur ces noeuds fils, en commencant par le premier
alphabetaCheckNodes(MaxAction, Player, [CurrentMovement | NextMovements], Alpha, Beta, BestMovement, BestValue, Depth) :-
    alphabeta(MaxAction, Player, CurrentMovement, Alpha, Beta, _, NodeValue, Depth),
    alphabetaCheckNextNodes(MaxAction, Player, CurrentMovement, NextMovements, Alpha, Beta, NodeValue, BestMovement, BestValue, Depth).
    
%% Regarde si les coupures alpha beta peuvent être utilisées, c'est-à-dire si on
%% doit continuer d'analyser les noeuds
alphabetaCheckNextNodes(_, _, CurrentMovement, [], _, _, NodeValue, CurrentMovement, NodeValue, _) :-
    !. % Il n'y a pas d'autres noeuds (mouvements) à analyser
    
alphabetaCheckNextNodes(MaxAction, _, CurrentMovement, _, Alpha, Beta, NodeValue, CurrentMovement, NodeValue, _) :-
    MaxAction, NodeValue < Alpha, !; % Coupure Alpha
    not(MaxAction), NodeValue > Beta, !. % Coupure Beta

%% Si aucune coupure n'a pu être appliquée, on analyse les noeuds restants
alphabetaCheckNextNodes(MaxAction, Player, CurrentMovement, NextMovements, Alpha, Beta, NodeValue, BestMovement, BestValue, Depth) :-
    % Mise à jour des bornes alpha et beta
    alphabetaCheckBounds(MaxAction, Alpha, Beta, NodeValue, NewAlpha, NewBeta),
    % Analyse des noeuds (mouvements)
    alphabetaCheckNodes(MaxAction, Player, NextMovements, NewAlpha, NewBeta, PotentialBestMovement, PotentialBestValue, Depth),
    % On regarde quelle solution est à retenir
    alphabetaCompareNodesSolutions(MaxAction, CurrentMovement, NodeValue, PotentialBestMovement, PotentialBestValue, BestMovement, BestValue).
    
% Mise à jour de la valeur Alpha : prend la valeur du noeud courant
alphabetaCheckBounds(MaxAction, Alpha, Beta, NodeValue, NodeValue, Beta) :-
    not(MaxAction), NodeValue > Alpha, !.
    
% Mise à jour de la valeur Beta : prend la valeur du noeud courant
alphabetaCheckBounds(MaxAction, Alpha, Beta, NodeValue, Alpha, NodeValue) :-
    MaxAction, NodeValue < Beta, !.
    
% Si les bornes Alpha et Beta ne changent pas (ie. si les autres prédicats
% alphabetaCheckBounds n'ont pas trouvé de correspondance).
alphabetaCheckBounds(_, Alpha, Beta, _, Alpha, Beta).

%% Regarde quel est le mouvement le meilleur, pour le noeud supérieur. Si on est
%% dans une action max, alors on recherche la plus petite valeur, que le noeud
%% supérieur recherche.
%% Cas où le mouvement 1 est meilleur que le 2
alphabetaCompareNodesSolutions(MaxAction, Movement1, Value1, _, Value2, Movement1, Value1) :-
    MaxAction, Value1 < Value2, !;
    not(MaxAction), Value1 > Value2, !.
    
%% Cas où le mouvement 2 est meilleur que le 1
alphabetaCompareNodesSolutions(_, _, _, Movement2, Value2, Movement2, Value2).
    
%% Passe de l'action min à max et inversement.
swapMinMax(MinMaxAction, NewMinMaxAction) :-
    MinMaxAction, NewMinMaxAction = false ;
    not(MinMaxAction), NewMinMaxAction = true.
    
    