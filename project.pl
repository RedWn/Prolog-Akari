:- use_module(kb).
:- use_module(ray_predicates).
:- use_module(cell_predicates).
:- use_module(print_utilities).

mark_list_as_lit([]).
mark_list_as_lit([cell(X, Y) | T]) :-
    assert(lit(cell(X, Y))),
    mark_list_as_lit(T).

% If length(NormalCells) == length(LitCells), then all cells are lit.
all_cells_lit :- 
    get_all_normal_cells(List),
    get_all_lit_cells(LitList),
    length(List, NormalCellsCount),
    length(LitList, LitCellsCount),
    NormalCellsCount =< LitCellsCount.


count_light_cells__([], Accumulator, Accumulator).
count_light_cells__([H | T], Accumulator, Count) :-
    (
        light(H) -> 
            NewAccumulator is Accumulator + 1,
            count_light_cells__(T, NewAccumulator, Count);
            count_light_cells__(T, Accumulator, Count)
    ).
count_light_cells(List, Count) :- count_light_cells__(List, 0, Count).

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
    all_cells_lit,
    no_double_light,
    light_count_correct.

solve:-
    solved,
    \+ print_grid_lit, nl,
    write("HAWKS TEAM IS STRONG"), !;
    (
        find_wall_num_that_have_equal_neighbors(List),
        length(List, N),
        N > 0,
        light_up_neighbors(List),
        solve
    );
    mark_unavailable_cells,
    light_up_singluar_cells,
    solve.

ray_contain_only_unavailable([]).
ray_contain_only_unavailable([cell(A, B) | T]):-
    findall(cell(X, Y),unavailable(cell(X, Y)), Unavailables),
    member(cell(A, B), Unavailables),
    ray_contain_only_unavailable(T).
    

find_all_singulars([], []).
find_all_singulars([cell(A,B)|T],Ans):-
    xray_of(cell(A,B),Xlist),
    yray_of(cell(A,B),Ylist),
    findall(cell(C,D),lit(cell(C,D)),List),
    subtract(Xlist, List, Xlist2),
    subtract(Ylist, List, Ylist2),
    length(Xlist2,N1),
    length(Ylist2,N2),
    \+((N1 is 0, N2 is 0);((N2 =\= 0, ray_contain_only_unavailable(Ylist2);
    N1 =\= 0, ray_contain_only_unavailable(Xlist2)))),find_all_singulars(T,Ans),!.

find_all_singulars([cell(A,B)|T],[cell(X,Y)|T1]):-
    X is A,Y is B,
    find_all_singulars(T,T1).

% Places a light in cell(X, Y) and marks it as lit.
% It also marks the x and y rays of the cell as lit.
add_light(cell(X, Y)):-
    assert(light(cell(X, Y))),
    assert(lit(cell(X, Y))),
    xray_of(cell(X, Y), Xlist),
    yray_of(cell(X, Y), Ylist),
    mark_list_as_lit(Xlist),
    mark_list_as_lit(Ylist).

light_up_list([]).
light_up_list([cell(X, Y) | T]) :-
    \+lit(cell(X, Y)),
    add_light(cell(X, Y)),
    light_up_list(T);
    light_up_list(T).

light_up_singluar_cells:-
    get_all_available_cells(Grid),
    find_all_singulars(Grid, List),
    light_up_list(List).

light_up_neighbors([]).
light_up_neighbors([cell(X, Y) | T]) :-
    all_neighbors_of(cell(X, Y), List),
    light_up_list(List),
    light_up_neighbors(T).


% Finds walls with numbers that have an equal number of neighbors
% and make sure those neighbors are unlit.
find_wall_num_that_have_equal_neighbors(List) :-
    findall(cell(X, Y), (
        wall_num(X, Y, GoalNumberOfLights),
        all_neighbors_of(cell(X, Y), NeighborsList),

        % FIXME: I don't think it's correct to search for all lit cells
        % in every iteration of this predicate. Perhaps we should move it to the top.
        get_all_lit_cells(LitList),

        subtract(NeighborsList, LitList, FinalList),
        length(FinalList, NumberOfNeighbors),
        get_adjacent_lights_count(cell(X, Y), ActualNumberOfLights),
        GoalNumberOfLights - ActualNumberOfLights  =:= NumberOfNeighbors,
        NumberOfNeighbors =\= 0

    ), List).
    
% Marks a list's members as unavailable (LIGHTS INCLUDED)
mark_list_cells_unavailable([]).
mark_list_cells_unavailable([cell(X, Y) | T]) :-
    assert(unavailable(cell(X, Y))),
    mark_list_cells_unavailable(T).

% Marks satisfied walls neighbours as unavailable (Zeroed walls included)
mark_satisfied_neighbours_as_unavailable_ :-
    wall_num(X, Y, _),
    check_for_lights_of_wall_num([cell(X, Y)]),
    all_neighbors_of(cell(X, Y), N),
    mark_list_cells_unavailable(N).

mark_satisfied_neighbours_as_unavailable :- 
    findall(_, mark_satisfied_neighbours_as_unavailable_, _).         

% Checks if a wall_num has GoalNumberOfLights + 1 available neighbors
% which are not lit cells.
wall_num_with_NPlusOne_available_neighbors(cell(X, Y)) :-
    wall_num(X, Y, GoalNumberOfLights),
    all_unlit_neighbors(cell(X, Y), UnlitNeighbors),
	length(UnlitNeighbors, UnlitNeighborsLength),
    get_adjacent_lights_count(cell(X, Y), ActualNumberOfLights),
	UnlitNeighborsLength =:= GoalNumberOfLights - ActualNumberOfLights + 1.

% Marks the cells that satisfy the algorithm's conditions as unavailable
mark_diag_as_unavailable__ :-
    % Get wall_num and one if its diagonal cells
    wall_num_with_NPlusOne_available_neighbors(cell(X, Y)),
	diagonal_neighbor_of(cell(X, Y), cell(A, B)),

    % Get the neighbors of wall_num and diagonal cell
	all_unlit_neighbors(cell(X, Y), NeighborsOfWallNum),
	all_neighbors_of(cell(A, B), NeighborsOfDiagonalCell),

    % Find the intersection of the two lists
	intersection(NeighborsOfWallNum, NeighborsOfDiagonalCell, SharedCells),
	length(SharedCells, SharedCellsLength),

    % If the length of the intersection == 2 then mark diagonal 
    % cell as unavailable.
	SharedCellsLength =:= 2,
	assert(unavailable(cell(A, B))).
						
mark_diag_as_unavailable :-
    findall(_, mark_diag_as_unavailable__, _). 

mark_unavailable_cells :- 
    mark_diag_as_unavailable,
    mark_satisfied_neighbours_as_unavailable.