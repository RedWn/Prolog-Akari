const https = require('https');

const fetchMap = (URL) => {
	return new Promise((resolve, reject) => {
		https
			.get(URL, (res) => {
				let data = '';
				console.log(
					`Fetching map from ${URL} ...\n`
				);

				res.on('data', (chunk) => (data += chunk));
				res.on('end', () => {
					// Returns `"puzz": "1...#..."`
					const puzzleStrip = data.match(/"puzz": ".*?"/gi)[0];

					// Returns `1...#...`
					const mapString = puzzleStrip
						.replace(/"puzz": "/gi, '')
						.replace(/"/gi, '');

					console.log(`Map string: ${mapString}`);
					resolve(mapString);
				});
			})
			.on('error', (err) => {
				console.log(err.message);
				reject(err.message);
			});
	});
};

module.exports = { fetchMap };
