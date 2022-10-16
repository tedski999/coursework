import fs from "fs";

// Parse envvars for configuration
const citiesFile: string = process.env.CITIES_FILE || console.error("CITIES_FILE not set!") || process.exit(1);
const maxResults: number = parseInt(process.env.MAX_SEARCH_RESULTS ?? "") || 50;

type City = {
	id: number;
	name: string;
	country: string;
};

class CityTrie {
	cities: City[] = [];
	children: Map<string, CityTrie> = new Map;
	public constructor(char = "", parent?: CityTrie) {
		if (parent) {
			parent.children.set(char, this);
		}
	}
}

function citySearch(prefix: string, country: string): City[]  {
	// Find the node corresponding to the search by name prefix
	let node: CityTrie = cities;
	for (const char of prefix.toLowerCase()) {
		node = node.children.get(char) ?? new CityTrie;
	}
	// Use breadth-first traversal to get the first n relevant results
	let results: City[] = [];
	const queue = [node];
	while (queue.length > 0 && results.length < maxResults) {
		node = queue.shift() ?? new CityTrie;
		results = results.concat(node.cities.filter(city => city.country.toLowerCase().indexOf(country) === 0));
		node.children.forEach(child => queue.push(child));
	}
	return results.slice(0, maxResults);
}

const cities: CityTrie = new CityTrie;
try {
	console.log("[Cities] Loading cities...");
	const data: string = fs.readFileSync(citiesFile, "utf-8");
	const loadedCities: City[] = JSON.parse(data);

	console.log(`[Cities] Loaded ${loadedCities.length} cities. Generating prefix search trie...`);
	loadedCities.forEach(city => {
		let node: CityTrie = cities;
		for (const char of city.name.toLowerCase()) {
			node = node.children.get(char) ?? new CityTrie(char, node);
		}
		node.cities.push(city);
	});

	console.log("[Cities] Done!");
} catch (err) {
	console.error("[Cities] Failed to load cities!", err)
	process.exit(1);
}

export { City, citySearch };
