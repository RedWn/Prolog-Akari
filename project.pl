:- use_module(kb).
:- use_module(ray_predicates).
:- use_module(cell_predicates).
<<<<<<< HEAD
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
:- dynamic unavailable_cell/2.

=======
>>>>>>> 6b1c4a891ebfd1ebb43f85478040829181341ec5

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
     L1 is Count,
      GoalNumberOfLights== L1 -> 
    light([cell(X,Y)|List]).



    % assert all neighbors of wall as lights.



%     nfirst(N, _, Lnew) :- N =< 0, Lnew = [].
%nfirst(_, [], []).
%nfirst(N, [X|Y], [X|Y1]) :- N1 is N - 1, nfirst(N1, Y, Y1).

%printList([]):-!.
%printList([H|T]):-printList(T), write(H), write('\n').
% mark_unavailable_cells:-
%     should follow the rules stated in the manifest but I am still to get a decent implementaion.
% light_up_singluar_cells:-
%     find every cell that has no neighbors and have no numbered and isnt a wall and assert it as light
% Lit arg controls if the normal tiles that are lit should be displayed lit or not
print_grid_cell(cell(X,Y),Lit):-(wall_num(X,Y,V),write(V),write(' '),!);
							(wall(X,Y),write(#),write(' '),!);
							(light(cell(X,Y)),write(*),write(' '),!);
							(is_cell_valid(cell(X,Y)),Lit=0,write(.),write(' '),!);
							(is_cell_valid(cell(X,Y)),Lit=1,is_cell_lighted(cell(X,Y)),write(+),write(' '),!);
							(is_cell_valid(cell(X,Y)),Lit=1,\+ is_cell_lighted(cell(X,Y)),write(.),write(' '),!).
							
% print_grid helpers
print_grid_(X, Y) :- size(W, _), X > W, X0 is 1,
					Y0 is Y - 1, nl, nl,
					print_grid_(X0, Y0).
					
print_grid_(X, Y) :- Y > 0, print_grid_cell(cell(X, Y), 0),
					X0 is X + 1, print_grid_(X0, Y).
% Prints the grid without lighting up normal tiles
print_grid :- size(_, H), print_grid_(1, H).
% print_grid_lit helpers
print_grid_lit_(X, Y) :- size(W, _), X > W, X0 is 1,
						Y0 is Y - 1, nl, nl,
						print_grid_lit_(X0, Y0).
						
print_grid_lit_(X, Y) :- Y > 0, print_grid_cell(cell(X, Y), 1),
						X0 is X + 1, print_grid_lit_(X0, Y).
% Prints the grid with lighting up normal tiles
print_grid_lit :- size(_, H), print_grid_lit_(1, H).