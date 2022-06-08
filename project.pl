:- use_module(kb).
:- use_module(ray_predicates).
:- use_module(cell_predicates).
:- use_module(print_utilities).

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

% Counts the number of lights in a given list.
% Exploration of kind count_light_cells(List, Result) is not working.
count_light_cells__([], Accumulator, Accumulator).
count_light_cells__([H | T], Accumulator, Result) :-
    (
        light(H) -> 
            NewAccumulator is Accumulator + 1,
            count_light_cells__(T, NewAccumulator, Result);
            count_light_cells__(T, Accumulator, Result)
    ).
count_light_cells(List, Result) :- count_light_cells__(List, 0, Result).

% Returns all the light cells and their count.
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

% check_for_lights_of_wall_num(([cell(2, 3), cell(4, 4)])). This case causes prolog to backtrack.
% Specifically, when a cell precedes the cell (4, 4).
light_count_correct :-
    findall(cell(X, Y), wall_num(X, Y, _), WallsWithNumbersList),
    check_for_lights_of_wall_num(WallsWithNumbersList).

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

ray_contain_only_unavailable([]).
ray_contain_only_unavailable([cell(A,B)|T]):-
    findall(cell(X,Y),unavailable(cell(X,Y)),Unavailables),
    member(cell(A,B), Unavailables),
    ray_contain_only_unavailable(T).
    

find_all_singulars([],[]).

find_all_singulars([cell(A,B)|T],Ans):-
    xray_of(cell(A,B),Xlist),
    yray_of(cell(A,B),Ylist),
    findall(cell(C,D),lit(cell(C,D)),List),
    subtract(Xlist, List, Xlist2),
    subtract(Ylist, List, Ylist2),
    length(Xlist2,N1),
    length(Ylist2,N2),
    \+((N1 is 0, N2 is 0);((N1 is 0,ray_contain_only_unavailable(Ylist2); N2 is 0,ray_contain_only_unavailable(Xlist2)))),find_all_singulars(T,Ans).

find_all_singulars([cell(A,B)|T],[cell(X,Y)|T1]):-
    X is A,Y is B,
    find_all_singulars(T,T1).

enlight([]).

enlight([cell(X,Y)|T]):-
    assert(lit(cell(X,Y))),
    enlight(T).

add_light(cell(X,Y)):-
    assert(light(cell(X,Y))),
    assert(lit(cell(X,Y))),
    xray_of(cell(X,Y),Xlist),
    enlight(Xlist),
    yray_of(cell(X,Y),Ylist),
    enlight(Ylist).

add_lights([]).

add_lights([cell(X,Y)|T]):-
    add_light(cell(X,Y)),
    add_lights(T).

light_up_singluar_cells:-
    get_all_available_cells(Grid),
    find_all_singulars(Grid,List),
    add_lights(List).

light_up_areas_of_walls_with_equal_neighbors:-
    wall_num(X, Y, GoalNumberOfLights),
    all_neighbors_of(cell(X, Y), List),
    length(List, NumberOfNeighbors),
    GoalNumberOfLights == NumberOfNeighbors,
    light_up_list(List).

light_up_list([]).
light_up_list([cell(X, Y) | T]) :-
    assert(light(cell(X, Y))),
    light_up_list(T).

% mark_unavailable_cells:-
%     should follow the rules stated in the manifest but I am still to get a decent implementaion.

% Marks a list's members as unavailable (LIGHTS INCLUDED)
mark_list_cells_unavailable([]).
mark_list_cells_unavailable([H|T]) :- H = cell(X, Y),
                                    assert(unavailable(cell(X, Y))),
                                    mark_list_cells_unavailable(T).

% Marks satesfied walls neighbours as unavailable (Zeroed walls included)
mark_satesfied_neighbours_as_unavailable_ :- wall_num(X, Y, _),
                                        check_for_lights_of_wall_num([cell(X, Y)]),
                                        all_neighbors_of(cell(X, Y), N),
                                        mark_list_cells_unavailable(N).

mark_satesfied_neighbours_as_unavailable:-findall(_, mark_satesfied_neighbours_as_unavailable_, _).         


wall_with_N_1_available_neighbours(cell(X,Y)) :- wall_num(X,Y,Z),
												get_adjacent_lights_count(cell(X,Y),LightsCount),
												all_neighbors_of(cell(X,Y),N),
												length(N,NCount),
												NCount - LightsCount =:= Z+1.
												
mark_diag_as_unavailable_:-wall_with_N_1_available_neighbours(cell(X,Y)),
						diag_neightbour_of(cell(X,Y),cell(A,B)),
						all_neighbors_of_without_lights(cell(X,Y),N0),
						all_neighbors_of(cell(A,B),N1),
						intersection(N0,N1,N2),
						length(N2,L),
						L=:=2,
						assert(unavailable(cell(A,B))).
						
mark_diag_as_unavailable:-findall(_,mark_diag_as_unavailable_,_). % TODO QA ME HARDER

mark_unavailable_cells:-mark_diag_as_unavailable,
                        mark_satesfied_neighbours_as_unavailable. % TODO QA ME TOO SENPAI ^-*