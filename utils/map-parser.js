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
 * Iterates over the characters of the map string and invokes
 * the `callback` parameter for each one.
 * @param {Array<String>} mapString
 * @param {Function} callback
 */
const iterateOverMap = (mapString, callback) => {
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

		callback({
			character,
			row: rowCounter,
			column: columnCounter,
		});

		rowCounter++;
	}
};

/**
 * Parses a given Akari string map into a list of predicates.
 *
 * @param mapInfo
 * @param {Boolean} includeLights Whether to include light predicates or not
 * @returns
 */
const parseMap = (mapInfo = {}, includeLights = false) => {
	let lightPredicates = [];
	const wallPredicates = [];
	const wallWithNumberPredicates = [];

	const { mapString, mapSolution } = mapInfo;
	const mapLimit = Math.sqrt(mapString.length);

	if (includeLights) lightPredicates = parseSolutions(mapSolution);

	iterateOverMap(mapString, ({ character, row, column }) => {
		const predicate = generatePredicate({
			character,
			row,
			column,
		});

		const type = predicate.type;
		if (type == predicateTypes.WALL) {
			wallPredicates.push(predicate.string);
		} else if (type == predicateTypes.WALL_NUMBER) {
			wallPredicates.push(predicate.additionalWallPredicate);
			wallWithNumberPredicates.push(predicate.string);
		}
	});

	const sizePredicate = `size(${mapLimit}, ${mapLimit}).\n`;
	return []
		.concat(lightPredicates)
		.concat(wallPredicates)
		.concat(wallWithNumberPredicates)
		.concat(sizePredicate);
};

const parseSolutions = (solutionsString) => {
	const lightPredicates = [];
	iterateOverMap(solutionsString, ({ character, row, column }) => {
		if (character == 1) {
			lightPredicates.push(`light(cell(${row}, ${column})).`);
		}
	});

	return lightPredicates;
};

// const writeTestsToTestFile = (predicates, filePath) => {
//     const tests = predicates.join('\n');

//     const testHeaders = `:- begin_tests(project).\n:- include(project).\n`;
//     const testBody = `
// test("Check if all lights are placed correctly") :-
//     ${tests}.
//     `;
//     const testFooter
// }

const writePredicatesToKB = (
	predicates = [],
	filePath = 'kb.pl',
	mapURL = ''
) => {
	const kb = predicates.join('\n');

	/**
	 * Please don't play with the strings below.
	 * Whitespaces are calculated in a very smart way so as
	 * not to mess up the readability of the knowledge base.
	 */
	const warningMessage = `% THIS FILE IS AUTO GENERATED. DO NOT EDIT DIRECTLY.\n`;
	const mapFromMessage = `% MAP URL: ${mapURL}\n`;
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
	const finalString = warningMessage
		.concat(mapFromMessage)
		.concat(moduleDefinition)
		.concat(kb)
		.concat(dynamicDefinitions);

	try {
		fs.writeFileSync(filePath, finalString);
	} catch (error) {
		console.error(error);
	}
};

const readUserInput = () => {
	return {
		mapURL: process.argv[2],
		shouldGenerateLightPredicates: process.argv[3] == 'yes',
	};
};

const main = async () => {
	const { mapURL, shouldGenerateLightPredicates } = readUserInput();
	if (!mapURL)
		return console.error('[ERROR]: Please provide a valid map URL.');

	const mapInfo = await fetchMap(mapURL);
	const predicates = parseMap(mapInfo, shouldGenerateLightPredicates);

	const kbFilePath = path.resolve(__dirname, `../kb.pl`);
	writePredicatesToKB(predicates, kbFilePath, mapURL);
    if (shouldGenerateLightPredicates) {
        console.log("Light predicates are included.\n");
    }
	console.log(`[SUCCESS]: Map written successfully to file kb.pl.`);
};

main();
