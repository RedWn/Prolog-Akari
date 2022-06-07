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

_Example string:_
`..1............2.#...1.3#....#.....2.#...0.2##.1....#.###.#...2.1.....1....#2.2...1.0............#..`

## TODO

-   [ ] Adjust variable names
-   [ ] Adjust whitespace
-   [ ] Solve project using _very advanced techniques_
-   [x] Use modules to hide private implementations
-   [ ] Write explanatory comments for complicated predicates
-   [ ] Write unit tests
