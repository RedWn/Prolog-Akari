:- module(cell_predicates, [
    is_cell_valid/1,
    x_indices_of_board/1,
    get_all_cells/1,
    get_all_available_cells/1,

    adjacent_to/2,
    neighbor_of/2,
    all_neighbors_of/2,
    neighbor_of_with_walls/2,
    all_neighbors_of_with_walls/2,
    diag_to/2,
    diag_neightbour_of/2,
    all_diag_neighbors_of/2,
    neighbor_of_without_lights/2,
    all_neighbors_of_without_lights/2
]).

% A cell is valid if it's positioned within the boundaries of the grid
is_cell_valid(cell(X, Y)) :-
    X >= 1, Y >= 1,
    size(Width, Height),
    X =< Width,
    Y =< Height. % NOTE: I think this can be just cell(X,Y) instead

% x indices, returns a list with x axes discrete values
x_indices__([], 0).
x_indices__(List, Index) :- 
    Index > 0,
    PreviousIndex is Index - 1,
    x_indices__(PreviousList, PreviousIndex),
    List = [Index | PreviousList].

x_indices_of_board(List):- size(Width, _), x_indices__(List, Width).

% y indices, returns a list with y axes discrete values
y_indices__([],0).
y_indices__(List, Index) :-
    Index > 0,
    PreviousIndex is Index - 1,
    y_indices__(PreviousList, PreviousIndex),
    List=[Index|PreviousList].

y_indices_of_board(List) :- size(_, Height), y_indices__(List, Height).

get_cell(X, Y) :-
    x_indices_of_board(XIndices),
    y_indices_of_board(YIndices),
    member(X, XIndices),
    member(Y, YIndices).

get_all_cells(List) :-
    findall(cell(X, Y), get_cell(X, Y), List).

get_all_available_cells(List) :-
    findall(cell(X, Y), get_cell(X, Y), All),
    findall(cell(X, Y), unavailable(cell(X, Y)), Unavailable),
    findall(cell(X, Y), wall(X, Y), Wall),
    findall(cell(X, Y), lit(cell(X, Y)), Lit),
    subtract(All, Unavailable, List1),
    subtract(List1, Lit, List2),
    subtract(List2, Wall, List).

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
    is_cell_valid(cell(A, B)),
    \+ wall(A, B).

% Find all the neighbors of a cell(X, Y) and put them in List           
all_neighbors_of(cell(X, Y), List) :- %TODO: update to only give unlit cells
    findall(cell(A, B), neighbor_of(cell(X, Y), cell(A, B)), List).

% Checks if two cells are neighbors.
neighbor_of_with_walls(cell(X, Y), cell(A, B)) :-
    is_cell_valid(cell(X, Y)),
    adjacent_to(cell(X, Y), cell(A, B)),
    is_cell_valid(cell(A, B)).

% Find all the neighbors of a cell(X, Y) and put them in List           
all_neighbors_of_with_walls(cell(X, Y), List) :- %TODO: update to only give unlit cells
    findall(cell(A, B), neighbor_of_with_walls(cell(X, Y), cell(A, B)), List).

% Checks if 2nd cell is on the diag of the 1st
diag_to(cell(X, Y), cell(A, B)) :-
    A is X + 1,
    B is Y + 1;
    
    A is X - 1,
    B is Y - 1;
        
    A is X + 1,
    B is Y - 1;
        
    A is X - 1,
    B is Y + 1.

% Checks if the two cells are on a diagonal
diag_neightbour_of(cell(X, Y), cell(A, B)) :-
    is_cell_valid(cell(X, Y)),
    diag_to(cell(X, Y), cell(A, B)),
    is_cell_valid(cell(A, B)),
    \+ wall(A, B).				

% Find all diagonal related cells and put them in List
all_diag_neighbors_of(cell(X, Y), List) :- 
    findall(cell(A, B), diag_neightbour_of(cell(X, Y), cell(A, B)), List).

% Same as checking neighbours, excluding light cells (NOT LIT)           
neighbor_of_without_lights(cell(X, Y), cell(A, B)) :-
    is_cell_valid(cell(X, Y)),
    adjacent_to(cell(X, Y), cell(A, B)),
    is_cell_valid(cell(A, B)),
    \+ wall(A, B),
    \+ light(cell(A, B)).
    
% Same as finding all neighbors, excluding light cells (NOT LIT)
all_neighbors_of_without_lights(cell(X, Y), List) :- 
    findall(cell(A, B), neighbor_of_without_lights(cell(X, Y), cell(A, B)), List).