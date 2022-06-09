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

## Parsing Maps

There's a map parser in the `utils` folder that accepts a map as a string and returns an array of the proper predicates. You can find such string representation of maps from the Akari game's website https://krazydad.com/play/akari/.

Just choose some map and open the browser's devtools. In the sources tab, inspect the main javascript folder and fiddle around a bit until you find this string.

_Example_:
`..1............2.#...1.3#....#.....2.#...0.2##.1....#.###.#...2.1.....1....#2.2...1.0............#..`

-   Numbers represent walls with numbers.
-   `.` represent normal cells.
-   `#` represent normal walls.

## Generating Maps

To generate a map for testing, you can follow these steps:

1. Get the map's string using the method mentioned [here](#parsing-maps).
2. Put the string of the map inside `utils/map.txt` (without quotes!).
3. Run `node utils/map-parser.js`
4. You will find the appropriate predicates written to a file called `kb.pl` in the root directory of the project.

## Knowledge base

-   `wall_num(X, Y, GoalNumberOfLights)`: Means that cell X, Y is a wall that has a number, `GoalNumberOfLights`. This number represents the number of lights that should be present around the cell. So `wall_num(2, 3, 4)` should have 4 light cells around it.
-   `wall(X, Y)`: Means that cell X, Y is a normal wall.
-   `size(Rows, Columns)`: Size of the game's grid. Note that the grid should be a **square**.
-   `lit(cell(X, Y))`: Means that cell X, Y is lit by a light.
-   `light(cell(X, Y))`: Means that cell X, Y is a light source.

## TODO

-   [ ] Adjust variable names
-   [ ] Adjust whitespace
-   [ ] Solve project using _very advanced techniques_
-   [x] Use modules to hide private implementations
-   [ ] Write explanatory comments for complicated predicates
-   [ ] Write unit tests
