const https = require('https');

const extractMapString = (html) => {
	// Returns `"puzz": "1...#..."`
	const puzzleStrip = html.match(/"puzz": ".*?"/gi)[0];
	// Returns `1...#...`
	const mapString = puzzleStrip.replace(/"puzz": "/gi, '').replace(/"/gi, '');
	return mapString;
};

const extractSolutionString = (html) => {
	// Returns `"solved": "1000101..."`
	const solutionStrip = html.match(/"solved": ".*?"/gi)[0];
	// Returns `100101..`
	const solutionString = solutionStrip
		.replace(/"solved": "/gi, '')
		.replace(/"/gi, '');
	return solutionString;
};

const fetchMap = (URL) => {
	return new Promise((resolve, reject) => {
		https
			.get(URL, (res) => {
				let data = '';
				console.log(`Fetching map from ${URL} ...\n`);

				res.on('data', (chunk) => (data += chunk));
				res.on('end', () => {
					const mapString = extractMapString(data);
					const mapSolution = extractSolutionString(data);

					console.log(`Map string: ${mapString}\n`);
					console.log(`Map solution: ${mapSolution}`);
					resolve({
                        mapString,
                        mapSolution
                    });
				});
			})
			.on('error', (err) => {
				console.log(err.message);
				reject(err.message);
			});
	});
};

module.exports = { fetchMap };
