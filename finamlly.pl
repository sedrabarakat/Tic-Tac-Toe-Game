:- use_module(library(clpfd)).

:- dynamic cellc/3.
:- dynamic rows/1.
:- dynamic cols/1.
:- dynamic array/1.
:- dynamic solve_cell/3.


size(4,4).
fixed_cell(1,4,x).
fixed_cell(2,2,o).
fixed_cell(2,3,o).



empty_cell(_,_,-).

%------------------------------------

cell(X,Y,S):- \+ fixed_cell(X,Y,_),solve_cell(X,Y,S);fixed_cell(X,Y,S);\+ solve_cell(X,Y,S),\+ fixed_cell(X,Y,S),empty_cell(X,Y,S).


%-------------------
print_grid :-
    size(Size,_),
    print_rows(1, Size).

print_rows(Irow, Size) :-
    Irow =< Size,
    print_columns(Irow, 1, Size),
    writeln(''),
    Irow1 is Irow + 1,
    print_rows(Irow1, Size).
print_rows(_, _).

print_columns(Irow, Jcol, Size) :-
    Jcol =< Size,
    cell(Irow, Jcol, Value),
    write(Value), write('|'),
    Jcol1 is Jcol + 1,
   print_columns(Irow, Jcol1, Size).
print_columns(_, _, _).

%-------------------
cells_collecting:- size(Row,_), between(1,Row, Irow),get_cell(Irow).

get_cell(Irow):-cellc(Irow,_,_),!; size(_,Col), between(1,Col, Icol), cell(Irow,Icol,Value),assert(cellc(Irow,Icol,Value)),Icol=\=Col,fail;
size(Rows,_),Irow=Rows,!.

add_Allcells_to_list:- findall(Value,cellc(_,_,Value),List),array_insert(List).

array_insert(List):-array(List),!;assert(array(List)).

%-------------------

% to play the game


%to check if the index is valid in terms of board size
valid_index(Irow,Jcol):-Irow>0, Jcol>0 ,size(Size,_),Irow=<Size,Jcol=<Size,fixed_cell(Irow,Jcol,_),write('this is a fixed cell, try another cell \n'),!;size(Size,_),Irow=<Size,Jcol=<Size.
% to check if the the value is either x or o, no other value
valid_value(Value):-Value=o;Value=x.


%to add values to the board and solve the game
insert_cell(Irow,Jcol,Value):- valid_index(Irow,Jcol),valid_value(Value),solve_cell(Irow,Jcol,_),retractall(solve_cell(Irow,Jcol,_)),assert(solve_cell(Irow,Jcol,Value)),print_grid,!;
valid_index(Irow,Jcol),valid_value(Value),assert(solve_cell(Irow,Jcol,Value)),print_grid,!.

%to reset the cell to the basic empty cell and delete value
reset_cell(Irow,Jcol,Value):-valid_index(Irow,Jcol),valid_value(Value),retractall(solve_cell(Irow,Jcol,Value)),print_grid.

%-------------------


%row lists
rowlists:-size(Size,_), array(List),member(_,List),split_list_into_lens(Size,List,Rowlists),assert(rows(Rowlists)).

%column lists
columnlists:- rows(Rowlists),transpose(Rowlists,Columnlists),assert(cols(Columnlists)).


%------------------------------------

split_list_into_lens(Len, Lst, Lsts) :-
    must_be(positive_integer, Len),
    split_list_into_lens_(Lst, Len, Lsts).

split_list_into_lens_([], _, []).
split_list_into_lens_([H|T], Len, [LstSplit|Lsts]) :-
    (   length(LstSplit, Len),
        append(LstSplit, LstRemainder, [H|T]) -> true
    ;   LstSplit = [H|T],
        length(LstSplit, LenSplitFinal),
        LenSplitFinal < Len,
        LstRemainder = [] ),
    split_list_into_lens_(LstRemainder, Len, Lsts).

%------------------------------------
%to get the column list at index Icol

column_at(Icol,Listcol):-cols(List), nth0(Icol,List,Listcol,_).

%to get the row list at index Irow

row_at(Irow,Listrow):-rows(List), nth0(Irow,List,Listrow,_).

%------------------------------------%check methods

no_triple:-rows(Rowlists),cols(Columnlists),check(Rowlists),check(Columnlists).

check(List):-maplist(no_three_consecutive,List).

tripple_check(H1,H2,H3):-H1=H2,H1\=H3;H1\=H2.
no_three_consecutive([]).
no_three_consecutive([_]).
no_three_consecutive([_, _]).
no_three_consecutive([H1, H2, H3|T]):-tripple_check(H1,H2,H3),no_three_consecutive([H2, H3|T]).

%-----------------------------
count_symbol(_, [], 0).
count_symbol(Element, [Element|Tail], Count) :-count_symbol(Element, Tail, TailCount),Count is TailCount + 1.
count_symbol(Element, [_|Tail], Count) :-count_symbol(Element, Tail, Count).

symbol_count_correct_check(Lists) :-
    maplist(count_symbol(x), Lists, CountsX),
    maplist(count_symbol(o), Lists, CountsO),
    sum_list(CountsX, Count1),!,
    sum_list(CountsO, Count2),!,
    Count1 = Count2.
symbol_count_correct:- rows(Rowlists),cols(Columnlists),symbol_count_correct_check(Rowlists),symbol_count_correct_check(Columnlists).
%----------------------------------------

same_length(Length,List):-length(List,Length).

all_cells_filled_check(Lists):-size(Size,_),same_length(Size,Lists),maplist(same_length(Size), Lists).

all_cells_filled:-rows(Rowlists),cols(Columnlists),all_cells_filled_check(Rowlists),all_cells_filled_check(Columnlists).
%----------------------------------------
no_repeat_check([]).
no_repeat_check(Lists) :-
    \+ (select(List, Lists, Rest), memberchk(List, Rest)).

no_repeat:-rows(Rowlists),cols(Columnlists),no_repeat_check(Rowlists),no_repeat_check(Columnlists).

%------------------------------------


%----------------------------------------
%to prepare the board for playing or for checking an existed solution
grid_ready:- cells_collecting,add_Allcells_to_list,rowlists,columnlists,!.

% when playing "refresh" method is used to update all lists refers to
% the board and its rows and columns so it stays updated to the new
% changes

refresh:-array(_),retractall(cellc(_,_,_)),retractall(array(_)),retractall(cols(_)),retractall(rows(_)),grid_ready.


%play mode....
play:- array(_),restart, grid_ready, write('\n'), print_grid,!; grid_ready, write('\n'), print_grid,!.

%check if solution is valid
solve:- refresh,all_cells_filled,symbol_count_correct,no_triple,no_repeat, write('your solution is right! how genius! :) ').

%if the solved cells is already added to KB as static
solved_grid:- array(List),\+ member(-,List),solve,print_grid.

% to play the game again you can restart the board using " restart"
% method

restart:- array(_),retractall(cellc(_,_,_)),retractall(array(_)),retractall(cols(_)),retractall(rows(_)),retractall(solve_cell(_,_,_)).
%--------------------



% Fill in the row with os and xs
fill_row([], _, _).
fill_row([H | T], Zeros, Ones) :-
    Zeros > 0,
    H = o,
    NextZeros is Zeros - 1,
    fill_row(T, NextZeros, Ones).
fill_row([H | T], Zeros, Ones) :-
    Ones > 0,
    H = x,
    NextOnes is Ones - 1,
    fill_row(T, Zeros, NextOnes).

% Check if the row has an equal number of os and xs
equal_numbers(Row) :-
    size(Size,_),
    HalfSize is Size // 2,
    fill_row(Row, HalfSize, HalfSize).


% Apply fixed_cell cells constraints to a single row
apply_fixed_cell_cells_row(_, [], _).
apply_fixed_cell_cells_row(RowIndex, [H | T], ColIndex) :-
    (fixed_cell(RowIndex, ColIndex, Value) ->
        H = Value
    ; true),
    NextColIndex is ColIndex + 1,
    apply_fixed_cell_cells_row(RowIndex, T, NextColIndex).

% Apply fixed_cell cells constraints to the entire matrix
apply_fixed_cell_cells([], _).
apply_fixed_cell_cells([Row | Matrix], RowIndex) :-
    apply_fixed_cell_cells_row(RowIndex, Row, 1),
    NextRowIndex is RowIndex + 1,
    apply_fixed_cell_cells(Matrix, NextRowIndex).

% Transpose a matrix
transpose([], []).
transpose([F | Fs], Ts) :-
    transpose(F, [F | Fs], Ts).

transpose([], _, []).
transpose([_ | Rs], Ms, [Ts | Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

% Separate the first elements and the remaining elements of a list of lists
lists_firsts_rests([], [], []).
lists_firsts_rests([[First | Rest] | Lists], [First | Firsts], [Rest | Rests]) :-
    lists_firsts_rests(Lists, Firsts, Rests).

% Solve the Tic Tac Logic game
solve(Grid) :-
    size(Size,_),
    length(Grid, Size),
    maplist(same_length(Size), Grid),
    maplist(equal_numbers, Grid),
    maplist(no_three_consecutive, Grid),
    no_repeat_check(Grid),
    transpose(Grid,TransposedGrid),
    maplist(no_three_consecutive, TransposedGrid),
    no_repeat_check(TransposedGrid),
    apply_fixed_cell_cells(Grid, 1).


% Print a row in the grid
print_row([]) :-
    nl.
print_row([H | T]) :-
    write(H),
    write(' '),
    print_row(T).
% Print the entire grid
print_grid([]).
print_grid([Row | T]) :-
    print_row(Row),
    print_grid(T).

% Solve the Tic Tac Logic game and print the solution
solve_and_print(Grid) :-
    solve(Grid),!,
    print_grid(Grid).


