:- module(print_utilities, [
    print_grid/0,
    print_grid_lit/0,
    print_grid_lit_/2
]).

% Lit arg controls if the normal tiles that are lit should be displayed lit or not
print_grid_cell(cell(X, Y), Lit) :-
    (wall_num(X, Y, V), write(V), write(' '), !);
    (wall(X, Y), write(#), write(' '), !);
    (light(cell(X, Y)), write('L'), write(' '), !);
    (is_cell_valid(cell(X, Y)), Lit=0, write(.), write(' '), !);
    (is_cell_valid(cell(X, Y)), Lit=1,lit(cell(X, Y)), write(+), write(' '), !);
    (is_cell_valid(cell(X, Y)), Lit=1,\+ lit(cell(X, Y)), write(.), write(' '), !).
							
% print_grid helpers
print_grid_(X, Y) :- 
    size(W, _), X > W, X0 is 1,
    Y0 is Y - 1, nl, nl,
    print_grid_(X0, Y0).
					
print_grid_(X, Y) :- 
    Y > 0, print_grid_cell(cell(X, Y), 0),
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