:- module(cell_predicates, [
    is_cell_valid/1,
    get_all_normal_cells/1,
    get_all_available_cells/1,
    get_all_lit_cells/1,
    all_neighbors_of/2,
    all_unlit_neighbors/2,
    diagonal_neighbor_of/2
]).

% A cell is valid if it's positioned within the boundaries of the grid.
is_cell_valid(cell(X, Y)) :-
    X >= 1, Y >= 1,
    size(Width, Height),
    X =< Width,
    Y =< Height. % NOTE: I think this can be just cell(X,Y) instead

% Returns a list of all possible discrete X indices of cells.
x_indices__([], 0).
x_indices__(List, Index) :- 
    Index > 0,
    PreviousIndex is Index - 1,
    x_indices__(PreviousList, PreviousIndex),
    List = [Index | PreviousList].

x_indices_of_grid(List):- 
    size(Width, _),
    x_indices__(List, Width).

% Returns a list of all possible discrete Y indices of cells.
y_indices__([],0).
y_indices__(List, Index) :-
    Index > 0,
    PreviousIndex is Index - 1,
    y_indices__(PreviousList, PreviousIndex),
    List=[Index|PreviousList].

y_indices_of_grid(List) :- 
    size(_, Height),
    y_indices__(List, Height).

get_cell(X, Y) :-
    % Loops over all possible X and Y combinations.
    x_indices_of_grid(XIndices),
    y_indices_of_grid(YIndices),
    member(X, XIndices),
    member(Y, YIndices).

% Returns all cells that are not walls or wall_nums or lights.
get_all_normal_cells(List) :-
    findall(
        cell(X, Y),
        (
            get_cell(X, Y),
            \+ wall(X, Y),
            \+ wall_num(X, Y, _),
            \+ light(cell(X, Y))
        ),
        List
    ).

get_all_available_cells(List) :-
    findall(cell(X, Y), get_cell(X, Y), All),
    findall(cell(X, Y), unavailable(cell(X, Y)), Unavailable),
    findall(cell(X, Y), wall(X, Y), Wall),
    findall(cell(X, Y), lit(cell(X, Y)), Lit),
    subtract(All, Unavailable, List1),
    subtract(List1, Lit, List2),
    subtract(List2, Wall, List).

get_all_lit_cells(List) :-
    findall(cell(X, Y), lit(cell(X, Y)), List).

% Checks if a certain cell is adjacent to another one.
% Note that diagonal cells aren't considered adjacent.
adjacent_to(cell(X, Y), cell(A, B)) :-
    A is X + 1,
    B is Y;
    
    A is X - 1,
    B is Y;
        
    A is X,
    B is Y + 1;
        
    A is X,
    B is Y - 1.

% Checks if two cells are neighbors.
neighbor_of(cell(X, Y), cell(A, B)) :-
    is_cell_valid(cell(X, Y)),
    adjacent_to(cell(X, Y), cell(A, B)),
    is_cell_valid(cell(A, B)).

% Find all the neighbors of a cell(X, Y) that are not walls.
all_neighbors_of(cell(X, Y), List) :- 
    findall(
        cell(A, B),
        (
            neighbor_of(cell(X, Y), cell(A, B)),
            \+ wall(A, B)
        ),
        List
    ).

% Find all the neighbors of a cell(X, Y) that are not lit.
all_unlit_neighbors(cell(X, Y), List) :-
    findall(
        cell(A, B),
        (
            neighbor_of(cell(X, Y), cell(A, B)),
            \+ wall(A, B),
            \+ lit(cell(A, B))
        ),
        List
    ).

% Checks if second cell is on one of the diagonals of the first cell.
diagonal_to(cell(X, Y), cell(A, B)) :-
    A is X + 1,
    B is Y + 1;
    
    A is X - 1,
    B is Y - 1;
        
    A is X + 1,
    B is Y - 1;
        
    A is X - 1,
    B is Y + 1.

% Checks if the two cells are on one diagonal.
diagonal_neighbor_of(cell(X, Y), cell(A, B)) :-
    is_cell_valid(cell(X, Y)),
    diagonal_to(cell(X, Y), cell(A, B)),
    is_cell_valid(cell(A, B)),
    \+ wall(A, B).				