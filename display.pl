init([
[ 1, 1, 0, 0, 0,-1,-1,-1,-1],
[ 1, 1, 0, 0, 0, 0,-1,-1,-1],
[ 1, 1, 1, 0, 0, 0, 0,-1,-1],
[ 1, 1, 1, 0, 0, 0, 0, 2,-1],
[ 1, 1, 1, 0, 0, 0, 2, 2, 2],
[-1, 1, 0, 0, 0, 0, 2, 2, 2],
[-1,-1, 0, 0, 0, 0, 2, 2, 2],
[-1,-1,-1, 0, 0, 0, 0, 2, 2],
[-1,-1,-1,-1, 0, 0, 0, 2, 2]
]).

play :-
    init(Board),
    displayBoard(Board).

displayBoard(Board):-
    nl, write('       A B C D EFGHI'),
    nl, write('       _________'),
    nl, displayRows(Board, 1),!, nl.

displayRows([], _).
displayRows([Row|Rest], RowNb):-
    write(RowNb), write('  '),
    NbSpace is abs(5-RowNb),
    displaySpaces(NbSpace),
    displayRow(Row, 1),
    write(' '), nl,
    NextRowNb is RowNb+1,
    displayRows(Rest, NextRowNb).

displayRow(_, 10).
displayRow([Element|Rest], ColNb):-
    displayPosition(Element),
    NextColNb is ColNb+1,
    displayRow(Rest, NextColNb).

displaySpaces(0).
displaySpaces(NbSpace):-
    write(' '),
    NbSpaceLeft is NbSpace-1,
    displaySpaces(NbSpaceLeft).

displayPosition(-1).
displayPosition(Element):-
    write(Element), write(' ').