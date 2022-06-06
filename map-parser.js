const exampleMap =
	'..1............2.#...1.3#....#.....2.#...0.2##.1....#.###.#...2.1.....1....#2.2...1.0............#..';

/**
 * Given a character from a map string, this function
 * generates the corresponding predicate for it.
 */
const generatePredicate = ({
    character = '.',
    row = 1,
    column = 1
}) => {
    let predicate = "";

    if (character == '.') {
        predicate = `cell(${row}, ${column})`;
    } else if (character == '#') {
        predicate = `wall(${row} ${column})`;
    } else {
        predicate = `wall_num(${row} ${column}, ${character})`;
    }

    return predicate;
}

const parseMap = (mapString = []) => {
    const prologPredicates = [];
    const mapLimit = Math.sqrt(mapString.length);

    let rowCounter = 1;
    let columnCounter = 1;
    
	for (const character of mapString) {
        // Reset column counter and move to another row
        // when you reach the end of the grid.
		if (columnCounter > mapLimit) {
			columnCounter = 1;
			rowCounter++;
		}
        
        const predicate = generatePredicate({
            character,
            row: rowCounter,
            column: columnCounter
        })

        prologPredicates.push(predicate);
        columnCounter++;
	}

    return prologPredicates;
};
