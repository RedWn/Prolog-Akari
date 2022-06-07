:- module(kb, [
    size/2,
    wall/2,
    wall_num/3,
    light/1
]).

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

:- dynamic light/1.
:- dynamic unavailable_cell/1.