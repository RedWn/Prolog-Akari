const exampleMap =
	'..1............2.#...1.3#....#.....2.#...0.2##.1....#.###.#...2.1.....1....#2.2...1.0............#..';

const predicateTypes = {
	CELL: 'CELL',
	WALL: 'WALL',
	WALL_NUMBER: 'WALL_NUMBER',
};

/**
 * Given a character from a map string, this function
 * generates the corresponding predicate for it.
 */
const generatePredicate = ({ character = '.', row = 1, column = 1 }) => {
	let string = '';
	let type = '';

	switch (character) {
		case '.':
			string = `cell(${row}, ${column})`;
			type = predicateTypes.CELL;
			break;

		case '#':
			string = `wall(${row} ${column})`;
			type = predicateTypes.WALL;
            break;

		default:
			string = `wall_num(${row} ${column}, ${character})`;
			type = predicateTypes.WALL_NUMBER;
            break;
	}

	return {
		string,
		type,
	};
};

const parseMap = (mapString = []) => {
	const cellPredicates = [];
	const wallPredicates = [];
	const wallWithNumberPredicates = [];

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
			column: columnCounter,
		});

		const type = predicate.type;
		if (type == predicateTypes.CELL) {
			cellPredicates.push(predicate);
		} else if (type == predicateTypes.WALL) {
			wallPredicates.push(predicate);
		} else {
			wallWithNumberPredicates.push(predicate);
		}

		columnCounter++;
	}

	return cellPredicates
		.concat(wallPredicates)
		.concat(wallWithNumberPredicates)
		.map((predicate) => predicate.string);
};
