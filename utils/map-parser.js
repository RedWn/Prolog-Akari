const fs = require('fs');
const path = require('path');
const { fetchMap } = require('./map-fetcher');

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
	let additionalWallPredicate = '';

	switch (character) {
		case '.':
			string = `cell(${row}, ${column}).`;
			type = predicateTypes.CELL;
			break;

		case '#':
			string = `wall(${row}, ${column}).`;
			type = predicateTypes.WALL;
			break;

		/**
		 * If the character represents a wall_num, then it needs two predicates:
		 * One for the wall_num, and another one for the wall.
		 **/
		default:
			string = `wall_num(${row}, ${column}, ${character}).`;
			additionalWallPredicate = `wall(${row}, ${column}).`;
			type = predicateTypes.WALL_NUMBER;
			break;
	}

	return {
		string,
		type,
		additionalWallPredicate,
	};
};

/**
 * Parses a given Akari string map into a list of predicates.
 *
 * @param {Array<String>} mapString
 * @param {Boolean} includeCells Whether to include cell predicates or not
 * @returns
 */
const parseMap = (mapString = [], includeCells = false) => {
	const cellPredicates = [];
	const wallPredicates = [];
	const wallWithNumberPredicates = [];
	const mapLimit = Math.sqrt(mapString.length);

	let rowCounter = 1;
	let columnCounter = mapLimit;

	/**
	 * ! IMPORTANT:
	 *
	 * Our map's (1, 1) coordinate is the bottom left corner,
	 * while maps from Akari's website start from top the top left corner.
	 *
	 * It's important to mind this indexing difference in
	 * row and column counters.
	 *
	 * i.e., for an 8 * 8 grid, Akari's map characters go like this:
	 *
	 * (1, 8) -> (2, 8) -> (3, 8), -> ... -> (8, 8)
	 * (1, 7) -> (2, 7) -> (3, 7), -> ... -> (8, 7)
	 *
	 */
	for (const character of mapString) {
		// Reset row counter and move to another row
		// when you reach the end of the grid.
		if (rowCounter > mapLimit) {
			rowCounter = 1;
			columnCounter--;
		}

		const predicate = generatePredicate({
			character,
			row: rowCounter,
			column: columnCounter,
		});

		const type = predicate.type;
		if (type == predicateTypes.CELL) {
			cellPredicates.push(predicate.string);
		} else if (type == predicateTypes.WALL) {
			wallPredicates.push(predicate.string);
		} else {
			wallPredicates.push(predicate.additionalWallPredicate);
			wallWithNumberPredicates.push(predicate.string);
		}

		rowCounter++;
	}

	const sizePredicate = `size(${mapLimit}, ${mapLimit}).\n`;
	const predicates = includeCells ? cellPredicates : [];
	return predicates
		.concat(wallPredicates)
		.concat(wallWithNumberPredicates)
		.concat(sizePredicate);
};

// TODO: Refactor the two parsing functions
const parseSolutions = (solutionsString) => {
    const lightPredicates = [];
	const mapLimit = Math.sqrt(solutionsString.length);

	let rowCounter = 1;
	let columnCounter = mapLimit;

	for (const character of mapString) {
		// Reset row counter and move to another row
		// when you reach the end of the grid.
		if (rowCounter > mapLimit) {
			rowCounter = 1;
			columnCounter--;
		}

        if (character == 1) {
            lightPredicates.push(`light(cell(${rowCounter}, ${columnCounter}))`);
        }

		rowCounter++;
	}

	return lightPredicates;
}

// const writeTestsToTestFile = (predicates, filePath) => {
//     const tests = predicates.join('\n');

//     const testHeaders = `:- begin_tests(project).\n:- include(project).\n`;
//     const testBody = `
// test("Check if all lights are placed correctly") :-
//     ${tests}.
//     `;
//     const testFooter
// }

const writePredicatesToKB = (predicates, filePath) => {
	const kb = predicates.join('\n');

	/**
	 * Please don't play with the strings below.
	 * Whitespaces are calculated in a very smart way so as
	 * not to mess up the readability of the knowledge base.
	 */
	const warningMessage = `% THIS FILE IS AUTO GENERATED. DO NOT EDIT DIRECTLY.\n`;
	const moduleDefinition = `:- module(kb, [
    size/2,
    wall/2,
    wall_num/3,
    light/1,
    unavailable/1,
    lit/1
]).\n\n`;

	const dynamicDefinitions = `
:- dynamic unavailable/1.
:- dynamic light/1.
:- dynamic lit/1.
`;

	try {
		fs.writeFileSync(filePath, warningMessage);
		fs.writeFileSync(filePath, moduleDefinition, { flag: 'a' });
		fs.writeFileSync(filePath, kb, { flag: 'a' });
		fs.writeFileSync(filePath, dynamicDefinitions, { flag: 'a' });
	} catch (error) {
		console.error(error);
	}
};

const readMapFromTextFile = (filePath) => {
	try {
		console.log(`\nReading map from ${filePath} ...\n`);
		const mapString = fs.readFileSync(filePath, 'utf8');
		return mapString;
	} catch (error) {
		console.error('Error reading map file.');
		console.error(error);
	}
};

const getMapURL = () => {
	return process.argv[2];
};

const main = async () => {
	const kbFilePath = path.resolve(__dirname, `../kb.pl`);

	// You can either read a map from a text file or fetch it online.
	const mapURL = getMapURL();
	if (!mapURL)
		return console.error('[ERROR]: Please provide a valid map URL.');

	const { mapString, mapSolution } = await fetchMap(mapURL);
	const predicates = parseMap(mapString);
    // const tests = parseSolution(mapSolution);

	writePredicatesToKB(predicates, kbFilePath);
    // writeTestsToTestFile()
	console.log(`Map written successfully to file kb.pl.`);
};

main();
