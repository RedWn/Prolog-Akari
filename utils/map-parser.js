const fs = require('fs');
const path = require('path');

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
			string = `wall(${row}, ${column})`;
			type = predicateTypes.WALL;
			break;

		default:
			string = `wall_num(${row}, ${column}, ${character})`;
			type = predicateTypes.WALL_NUMBER;
			break;
	}

	return {
		string,
		type,
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

	const sizePredicate = {
		string: `size(${mapLimit}, ${mapLimit})`,
	};
	const predicates = includeCells ? cellPredicates : [];
	return predicates
		.concat(wallPredicates)
		.concat(wallWithNumberPredicates)
		.concat(sizePredicate)
		.map((predicate) => predicate.string);
};

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

/**
 * Refines predicates to match prolog's syntax.
 */
const refinePredicates = (predicates) =>
	predicates.map((predicate) => `${predicate}.`);

const readMapFromFile = (filePath) => {
	try {
		console.log(`\nReading map from ${filePath} ...\n`);
		const mapString = fs.readFileSync(filePath, 'utf8');
		return mapString;
	} catch (error) {
		console.error('Error reading map file.');
		console.error(error);
	}
};

const main = () => {
	// File paths and names
	const kbFilename = 'kb.pl';
	const kbFilePath = path.resolve(__dirname, `../${kbFilename}`);
	const mapFilePath = path.join(__dirname, '/map.txt');

    // Actual parsing
	const mapString = readMapFromFile(mapFilePath);
	const predicates = parseMap(mapString);
	const refinedPredicates = refinePredicates(predicates);
	writePredicatesToKB(refinedPredicates, kbFilePath);

	console.log(`Predicates written successfully to file ${kbFilename}.`);
};

main();
