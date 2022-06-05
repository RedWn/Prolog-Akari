:- ensure_loaded(xy_ray).
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
light(cell(1, 1)).
light(cell(1, 2)).
light(cell(1, 3)).
light(cell(4, 3)).
light(cell(1, 5)).
light(cell(1, 6)).
light(cell(1, 7)).
light(cell(1, 8)).
light(cell(2, 1)).
light(cell(2, 2)).
light(cell(2, 3)).
light(cell(2, 4)).
light(cell(2, 5)).
light(cell(2, 6)).
light(cell(2, 7)).
light(cell(2, 8)).
light(cell(3, 1)).
light(cell(3, 2)).
light(cell(3, 3)).
light(cell(3, 4)).
light(cell(3, 5)).
light(cell(3, 6)).
light(cell(3, 7)).
light(cell(3, 8)).
light(cell(4, 1)).
light(cell(4, 2)).
light(cell(4, 3)).
light(cell(4, 4)).
light(cell(4, 5)).
light(cell(4, 6)).
light(cell(4, 7)).
light(cell(4, 8)).
light(cell(5, 1)).
light(cell(5, 2)).
light(cell(5, 3)).
light(cell(5, 4)).
light(cell(5, 5)).
light(cell(5, 6)).
light(cell(5, 7)).
light(cell(5, 8)).
light(cell(6, 1)).
light(cell(6, 2)).
light(cell(6, 3)).
light(cell(6, 4)).
light(cell(6, 5)).
light(cell(6, 6)).
light(cell(6, 7)).
light(cell(6, 8)).
light(cell(7, 1)).
light(cell(7, 2)).
light(cell(7, 3)).
light(cell(7, 4)).
light(cell(7, 5)).
light(cell(7, 6)).
light(cell(7, 7)).
light(cell(7, 8)).
light(cell(8, 1)).
light(cell(8, 2)).
light(cell(8, 3)).
light(cell(8, 4)).
light(cell(8, 5)).
light(cell(8, 6)).
light(cell(8, 7)).

:-dynamic light/2.

% A cell is valid if it's positioned within the boundaries of the grid
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
    is_cell_valid(A, B),
    \+ wall(A, B).

% Find all the neighbors of a cell(X, Y) and put them in List           
all_neighbors_of(cell(X, Y), List) :- 
    findall(cell(A, B), neighbor_of(cell(X, Y), cell(A, B)), List).

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

% Returns all the lights in L and their count in C
get_all_light_cells(L,C):-findall(cell(X,Y),light(cell(X,Y)),L),length(L, C).

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

get_adjacent_lights_number(cell(X,Y),N):-
    all_neighbors_of(cell(X,Y),ListOfNeighbors),
    findall(cell(X1,Y1), light(cell(X1,Y1)), ListofLights),
    intersection(ListOfNeighbors, ListofLights, List),
    length(List, N).

does_wall_cell_have_enough_lights(cell(X, Y)):-
    wall_num(X,Y,N),
    get_adjacent_lights_number(cell(X,Y),N1),
    N =:= N1.
    

 all_cells_lighted(cell(X,Y)) :- % cell(1,1)
     X<8->X1 is X+1, 
     all_cells_lighted(cell(X1,Y));
     Y<8->Y1 is Y+1,
     all_cells_lighted(cell(X,Y1)); % waiting for hassan 
    light(cell(X,Y)). 
% Returns true if there is more than one light in a single cross (with respect to rays)
% in the whole grid
plenty_lights_within_cross:-light(X),xray_of(X,Lx),count_light_cells(Lx,C1)
                                    ,yray_of(X,Ly),count_light_cells(Ly,C2)
                                    ,C1+C2>0.

% Returns true if there is no two lights (or more) in a single cross (with respect to rays)
% in the whole grid
no_double_light:- \+ plenty_lights_within_cross.


check_for_all_lights([]).
check_for_all_lights([cell(X,Y)|T]):-
    does_wall_cell_have_enough_lights(cell(X, Y)),
    check_for_all_lights(T).


light_count_correct:-
    findall(cell(X,Y), wall_num(X,Y,Z), L),
    check_for_all_lights(L).

% solved :-
%     all_cells_lighted,
%     no_double_light,
%     light_count_correct .