:- use_module(ray_predicates).
% Size of the game's grid
% size(NumberOfRows, NumberOfColumns).
% Note that indexing starts from 1 for both rows and columns.
size(8, 8).

% These cells are walls without numbers
% wall(XPosition, YPosition).
wall(3, 1).
wall(6, 2).
wall(7, 2).
wall(2, 3).
wall(4, 4).
wall(8, 4).
wall(1, 5).
wall(5, 5).
wall(7, 6).
wall(2, 7).
wall(3, 7).
wall(6, 8).

% These cells are walls that contain numbers.
% wall_num(XPosition, YPosition, NumberOfAdjacentLights).
wall_num(6, 2, 1).
wall_num(2, 3, 2).
wall_num(4, 4, 4).
wall_num(8, 4, 0).
wall_num(7, 6, 0).
wall_num(2, 7, 3).
wall_num(6, 8, 1).

% Light Cells (for testing purposes)
light(cell(6, 1)).
light(cell(2, 2)).
light(cell(8, 2)).
light(cell(1, 3)).
light(cell(4, 3)).
light(cell(3, 4)).
light(cell(5, 4)).
light(cell(4, 5)).
light(cell(6, 5)).
light(cell(2, 6)).
light(cell(1, 7)).
light(cell(8, 7)).
light(cell(2, 8)).
light(cell(7, 8)).

:- dynamic light/2.

% A cell is valid if it's positioned within the boundaries of the grid
is_cell_valid(cell(X, Y)) :-
    X >= 1, Y >= 1,
    size(Width, Height),
    X =< Width,
    Y =< Height.

% x indices, returns a list with x axes discrete values
x_indices_private([],0).
x_indices_private(L,I):- I0 is I-1, x_indices_private(L0,I0), L=[I|L0].
x_indices(L):- size(X,_), x_indices_private(L,X).

% y indices, returns a list with y axes discrete values
y_indices_private([],0).
y_indices_private(L,I):- I0 is I-1, y_indices_private(L0,I0), L=[I|L0].
y_indices(L):- size(_,Y), y_indices_private(L,Y).

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
all_neighbors_of(cell(X, Y), List) :- 
    findall(cell(A, B), neighbor_of(cell(X, Y), cell(A, B)), List).

% TODO: Requires re-writing
 all_cells_lighted :- 
    cell(X, Y),
    is_cell_valid(cell(X, Y)),
     X < 8 -> X1 is X+1, 
     
    Y<8->Y1 is Y+1.


% Counts the number of lights in a given list
% exploration of kind count_light_cells(L,R) is not working
count_light_cells__([], Accumulator, Accumulator).
count_light_cells__([H | T], Accumulator, Result) :-
    (
        light(H) -> 
            NewAccumulator is Accumulator + 1,
            count_light_cells__(T, NewAccumulator, Result);

            count_light_cells__(T, Accumulator, Result)
    ).

count_light_cells(List, Result) :- count_light_cells__(List, 0, Result).

% Returns all the lights in List and their count in Count
get_all_light_cells(List, Count) :-
    findall(cell(X,Y), light(cell(X, Y)), List),
    length(List, Count).

is_cell_lighted(cell(X, Y)) :- light(cell(X, Y)), !.
is_cell_lighted(cell(X, Y)) :-
    xray_of(cell(X, Y), XRay),
    yray_of(cell(X, Y), YRay),
    count_light_cells(XRay, LightsInXRay),
    count_light_cells(YRay, LightsInYRay),
    (
        LightsInXRay > 0, !;
        LightsInYRay > 0
    ).

% Returns true if there is more than one light in a single axis in the whole grid.
more_than_one_light_in_axis :- 
    % Get a random light
    light(Cell),

    % Fetch X and Y rays and count lights in them.
    xray_of(Cell, XRay),
    yray_of(Cell, YRay),
    count_light_cells(XRay, LightsCountInXRay),
    count_light_cells(YRay, LightsCountInYRay),

    LightsCountInXRay + LightsCountInYRay >= 1.


no_double_light :- \+ more_than_one_light_in_axis.

get_adjacent_lights_count(cell(X, Y), Count) :-
    all_neighbors_of(cell(X, Y), AdjacentLightsList),
    count_light_cells(AdjacentLightsList, Count).

does_wall_cell_have_enough_lights(cell(X, Y)) :-
    wall_num(X, Y, GoalNumberOfLights),
    get_adjacent_lights_count(cell(X, Y), ActualNumberOfLights),
    GoalNumberOfLights =:= ActualNumberOfLights.

% Iterates over all walls with light numbers
% and checks if the number of adjacent lights is correct.
check_for_lights_of_wall_num([]).
check_for_lights_of_wall_num([cell(X, Y) | T]) :-
    does_wall_cell_have_enough_lights(cell(X, Y)),
    check_for_lights_of_wall_num(T).

light_count_correct :-
    findall(cell(X,Y), wall_num(X, Y, _), WallsWithNumbersList),
    check_for_lights_of_wall_num(WallsWithNumbersList).

% solved :-
%     all_cells_lighted,
%     no_double_light,
%     light_count_correct .
