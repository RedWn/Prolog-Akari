% Size of the game's grid
% size(NumberOfRows, NumberOfColumns).
% Note that indexing starts from 1 for both rows and columns.
size(8,8).

% These cells are walls without numbers
% wall(XPosition, YPosition).
wall(4,1).
wall(4,5).
wall(5,4).
wall(5,8).
wall(1,6).
wall(2,2).
wall(2,3).
wall(3,7).
wall(6,2).
wall(7,6).
wall(7,7).
wall(8,3).

% These cells are walls that contain numbers.
% wall_num(XPosition, YPosition, NumberOfAdjacentLights).
wall_num(1,6,1).
wall_num(2,2,3).
wall_num(3,7,0).
wall_num(5,4,4).
wall_num(5,8,0).
wall_num(6,2,2).
wall_num(7,6,1).

% A cell is valid if it's positioned within the boundaries of the grid.
is_cell_valid(X, Y) :-
    X >= 1, Y >= 1,
    size(Width, Height),
    X =< Width,
    Y =< Height,\+wall(X,Y).

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
    is_cell_valid(X, Y),
    adjacent_to(cell(X, Y), cell(A, B)),
    is_cell_valid(A, B).
           

all_neighbors_of(cell(X, Y), List) :- 
    findall(cell(A, B), neighbor_of(cell(X, Y), cell(A, B)), List).

% Returns all valid cells to the left and the right
xray_forward_of(cell(X,Y),cell(A,B)):-
    X1 is X+1,
    is_cell_valid(X1, Y),(
    A is X1,
    B is Y;

    X1 is X+1,
    xray_forward_of(cell(X1,Y),cell(A,B))).

xray_backward_of(cell(X,Y),cell(A,B)):-
    X1 is X-1,
    is_cell_valid(X1, Y),(
    A is X1,
    B is Y;

    X1 is X-1,
    xray_backward_of(cell(X1,Y),cell(A,B))).


xray_of(cell(X,Y),List):-
    findall(cell(A, B), xray_forward_of(cell(X, Y), cell(A, B)), List1),
    findall(cell(A, B), xray_backward_of(cell(X, Y), cell(A, B)), List2),
    append(List1,List2,List).


% Returns all valid cells under and above
yray_forward_of(cell(X,Y),cell(A,B)):-
    Y1 is Y+1,
    is_cell_valid(X, Y1),(
    A is X,
    B is Y1;

    Y1 is Y+1,
    yray_forward_of(cell(X,Y1),cell(A,B))).

yray_backward_of(cell(X,Y),cell(A,B)):-
    Y1 is Y-1,
    is_cell_valid(X, Y1),(
    A is X,
    B is Y1;

    Y1 is Y-1,
    yray_backward_of(cell(X,Y1),cell(A,B))).


yray_of(cell(X,Y),List):-
    findall(cell(A, B), yray_forward_of(cell(X, Y), cell(A, B)), List1),
    findall(cell(A, B), yray_backward_of(cell(X, Y), cell(A, B)), List2),
    append(List1,List2,List).

% Counts the number of lights in a given list
% count_light_cells([], 0).
% count_light_cells(List, Count) :-
%     List = [H | T],
%     light(H),
%     NewCount is Count + 1,
%     count_light_cells(T).

% is_cell_lighted(cell(X, Y)) :-
%     approach 1: each time we store a new light bulb
%     we indicate that cell(X, Y) is either lighted or not.

%     approach 2: foreach light in the game, check both its
%     x and y rays if they can light up the given cell.
%     Don't forget that if the cell in itself is a light, then it's lighted

% does_wall_cell_have_enough_lights(cell(X, Y)) :-
%     wall_num(X, Y, NumberOfLights),
%     get_all_adjacent_lights(List),
%     List.length == NumberOfLights.

% all_cells_lighted :-
%     foreach cell in grid,
%     is_cell_lighted(cell).

% no_double_light :-
%     foreach light in game,
%     check if there's another light in its x or y rays.

% light_count_correct :-
%     foreach wall_with_number,
%     does_wall_cell_have_enough_lights.

% solved :-
%     all_cells_lighted,
%     no_double_light,
%     light_count_correct .