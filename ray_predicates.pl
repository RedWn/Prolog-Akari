:- module(ray_predicates, [
    xray_of/2,
    yray_of/2
]).

xray_forward_of(cell(X, Y), cell(A, B)) :-
    X1 is X + 1,
    is_cell_valid(cell(X1, Y)),
    \+ wall(X1, Y),
    (
        A is X1,
        B is Y;

        X1 is X + 1,
        xray_forward_of(cell(X1, Y), cell(A, B))
    ).

xray_backward_of(cell(X, Y), cell(A, B)) :-
    X1 is X - 1,
    is_cell_valid(cell(X1, Y)),
    \+ wall(X1, Y),
    (
        A is X1,
        B is Y;

        X1 is X - 1,
        xray_backward_of(cell(X1, Y), cell(A, B))
    ).

% Returns all valid cells to the left and right of cell(X, Y)
% until a wall is found.
xray_of(cell(X, Y), List) :-
    findall(cell(A, B), xray_forward_of(cell(X, Y), cell(A, B)), Right),
    findall(cell(A, B), xray_backward_of(cell(X, Y), cell(A, B)), Left),
    append(Right, Left, List).


% Returns all valid cells under and above
yray_forward_of(cell(X, Y), cell(A, B)) :-
    Y1 is Y + 1,
    is_cell_valid(cell(X, Y1)),
    \+ wall(X, Y1),
    (
        A is X,
        B is Y1;

        Y1 is Y+1,
        yray_forward_of(cell(X, Y1), cell(A, B))
    ).

yray_backward_of(cell(X, Y), cell(A, B)) :-
    Y1 is Y - 1,
    is_cell_valid(cell(X, Y1)),
    \+ wall(X, Y1),
    (
        A is X,
        B is Y1;

        Y1 is Y - 1,
        yray_backward_of(cell(X, Y1), cell(A, B))
    ).

% Returns all valid cells above and below cell(X, Y)
% until a wall is found.
yray_of(cell(X, Y), List) :-
    findall(cell(A, B), yray_forward_of(cell(X, Y), cell(A, B)), Above),
    findall(cell(A, B), yray_backward_of(cell(X, Y), cell(A, B)), Below),
    append(Above, Below, List).
