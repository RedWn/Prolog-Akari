# Prolog-Akari -- Hawks Team's Version

## Running The Game

TODO

## Conventions

Predicates that end with `__` are considered private and should not be exposed as public APIs.

## Unit Testing

Here's a guide that is actually readable on unit testing in prolog: http://pbrown.me/blog/swi_prolog_unit_testing_env/

**Summary**:

1. Load the project's file using SWI-Prolog's CLI command `swipl project.pl`
2. Type `load_test_files([make(all)]).`
3. Type `run_tests.` to actually run tests
4. Whenever you want to run tests again, type `make.`

In the `utils` folder, you can find an example map that you can work on.

## Maps

There's a map parser in the `utils` folder that can do the following:

1.  Fetch an Akari map from https://krazydad.com/play/akari/, parse it, and write it to `kb.pl`.
2.  Parse a string map from `utils/map.txt` and write it to `kb.pl`.

Its current implementation does the first thing, but you can fiddle around a bit with the code in `utils/map-parser.js` to read a map from a text file. (The function `readMapFromFile` exists, you just have to invoke it).

### Example of a string map

This is a string representation of an 8 x 8 grid.

`..1............2.#...1.3#....#.....2.#...0.2##.1....#.###.#...2.1.....1....#2.2...1.0............#..`

-   Numbers represent walls with numbers.
-   `.` represent normal cells.
-   `#` represent normal walls.

### How generating maps works

The parser first fetches the whole HTML string of the webpage, and then applies a regular expression against it so it can extract the string representation of the map. The string of the map exists in the HTML source code, you just have to find it.

If you're curious, you can inspect the source code of Akari's website using your browser's devtools.

### Invoking the parser

First, make sure that you have `Node` installed on your machine.

1.  Copy the URL of the map that you want.
2.  Run the map parser and pass it the URL of the map like this: `node utils/map-parser.js "https://krazydad.com/play/akari/?kind=8x8&volumeNumber=2&bookNumber=8&puzzleNumber=16"`  
    **Note that the URL has to be double quoted**

3.  You will find the appropriate predicates written to a file called `kb.pl` in the root directory of the project.

## Knowledge base

-   `wall_num(X, Y, GoalNumberOfLights)`: Means that cell X, Y is a wall that has a number, `GoalNumberOfLights`. This number represents the number of lights that should be present around the cell. So `wall_num(2, 3, 4)` should have 4 light cells around it.
-   `wall(X, Y)`: Means that cell X, Y is a normal wall.
-   `size(Rows, Columns)`: Size of the game's grid. Note that the grid should be a **square**.
-   `lit(cell(X, Y))`: Means that cell X, Y is lit by a light.
-   `light(cell(X, Y))`: Means that cell X, Y is a light source.

## Graphically Represented Results
You can use our provided C++ project which in turn runs a SWI-Prolog instance within it, that relies on "project.pl" file, to see the puzzle before and after -if the algorithm could solve- solving.
## Building:
Make sure you have the following dependencies:
- SFML development libraries
- glbinding development libraries
- SWI-Prolog development library
With these installed you can build using the provided Meson script.
## Running:
Run the application with "project.pl" being the second command line argument.
Press the space button and the algorithm will attempt to solve the puzzle.
In case the algortithm failed to solve the puzzle, it will basically freeze the app until you close it.

P.S : All the provided textures are public domain.

## Solution Algorithm

TODO: @redwn please elaborate on the algorithm here.

## TODO

-   [ ] Adjust variable names
-   [ ] Adjust whitespace
-   [ ] Solve project using _very advanced techniques_
-   [x] Use modules to hide private implementations
-   [x] Write explanatory comments for complicated predicates
-   [ ] Write unit tests
