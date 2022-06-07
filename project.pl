:- use_module(kb).
:- use_module(ray_predicates).
:- use_module(cell_predicates).
:- use_module(print_utilities).
% Size of the game's grid
% size(NumberOfRows, NumberOfColumns).
% Note that indexing starts from 1 for both rows and columns.


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
is_list_lighted([]).
is_list_lighted([cell(X, Y) | T]) :-
    is_cell_lighted(cell(X, Y)),
    is_list_lighted(T).
 all_cells_lighted :- 
    get_all_cells(List),
    is_list_lighted(List).
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
% Returns true if there is only one light in every single axis in the grid.
no_double_light :- 
    \+ (
        % Get a random light
        light(Cell),
        % Fetch X and Y rays and count lights in them.
        xray_of(Cell, XRay),
        yray_of(Cell, YRay),
        count_light_cells(XRay, LightsCountInXRay),
        count_light_cells(YRay, LightsCountInYRay),
        LightsCountInXRay + LightsCountInYRay >= 1
    ).
get_adjacent_lights_count(cell(X, Y), Count) :-
    all_neighbors_of(cell(X, Y), AdjacentLightsList),
    count_light_cells(AdjacentLightsList, Count).
    
% Iterates over all walls with light numbers
% and checks if the number of adjacent lights is correct.
check_for_lights_of_wall_num([]).
check_for_lights_of_wall_num([cell(X, Y) | T]) :-
    wall_num(X, Y, GoalNumberOfLights),
    get_adjacent_lights_count(cell(X, Y), ActualNumberOfLights),
    GoalNumberOfLights =:= ActualNumberOfLights,
    
    check_for_lights_of_wall_num(T).
light_count_correct :-
    findall(cell(X, Y), wall_num(X, Y, _), WallsWithNumbersList),
    check_for_lights_of_wall_num(WallsWithNumbersList).
% check_for_lights_of_wall_num(([cell(2, 3), cell(4, 4)])). This case causes prolog to backtrack.
% Specifically, when a cell precedes the cell (4, 4).
solved :-
    all_cells_lighted,
    no_double_light,
    light_count_correct.
% the algorithm:
% solve:-
%     solved,findall(lights),print(lights);(
%     fill_wall_with_equal_neighbors,
%     solve
%     );
%     mark_unavailable_cells,
%     solve;
%     light_up_singluar_cells,
%     solve.
 get_wall_with_equal_neighbors:-
     wall_num(X, Y, GoalNumberOfLights),
     all_neighbors_of(cell(X, Y), List),
     length(List,Count),
      GoalNumberOfLights==Count,
    light_up_list(List).

    light_list([]).
    light_list([cell(X,Y)|List]):-
    light(cell(X,Y)),
    light_list(List).

% mark_unavailable_cells:-
%     should follow the rules stated in the manifest but I am still to get a decent implementaion.
% light_up_singluar_cells:-
%     find every cell that has no neighbors and have no numbered and isnt a wall and assert it as light
% Lit arg controls if the normal tiles that are lit should be displayed lit or not
