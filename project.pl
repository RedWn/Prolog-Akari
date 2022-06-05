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


% Light Cells (for testing purposes)
light(cell(2,2)).
light(cell(1,3)).
light(cell(6,1)).
light(cell(8,2)).


% A cell is valid if it's positioned within the boundaries of the grid.
is_cell_valid(X, Y) :-
    X >= 1, Y >= 1,
    size(Width, Height),
    X =< Width,
    Y =< Height.

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

% xray_of(cell(X, Y), L) :-
%     X1 is X + 1,
%     size(Width, Height),
%     X1 < Width,
%     \+ wall(X1,Y),
%     append([], [X1,Y],L1),
%     append(L,L1,L),
%     xray_of(X1,Y,L);
%     !.

% Counts the number of lights in a given list
% exploration of kind count_light_cells(L,R) is not working (yet)
count_light_cells([],0).
count_light_cells([H|T],R):-count_light_cells(T,R0),(light(H)->R is R0+1;R is R0).

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