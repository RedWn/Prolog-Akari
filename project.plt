:- begin_tests(project).
:- include(project).

test("Cell (6, 1) should be lighted") :-
    light(cell(6, 1)).

test("Cell (6, 2) should not be lighted") :-
    \+ light(cell(6, 2)).

test("Should return the right list of cell(8, 8)'s neighbors") :-
    all_neighbors_of(cell(8, 8), List),
    List = [cell(7, 8), cell(8, 7)].

test("Should return the right list of cell(1, 1)'s neighbors") :-
    all_neighbors_of(cell(1, 1), List),
    List = [cell(2, 1), cell(1, 2)].

:- end_tests(project).