import { createApp } from "https://unpkg.com/vue@3/dist/vue.esm-browser.prod.js"

const app = {
	data() {
		return {
			search: "",
			results: [],
			city: null,
			cityData: null
		}
	},
	methods: {
		onSearchInput: onSearchInput,
		selectCity: selectCity,
		clearCity: clearCity
	}
}

createApp(app).mount("#app")

async function onSearchInput() {
	if (this.search.length === 0) {
		this.results = [];
		return;
	}
	// Split search into city and country
	let searchComponents = this.search.split(",", 2);
	let city = (searchComponents[0] ?? "").trim();
	let country = (searchComponents[1] ?? "").trim();
	try {
		// Query API for search results
		const response = await fetch(`/v1/search?city=${city}&country=${country}`)
		this.results = response.ok ? await response.json() : [];
	} catch (err) {
		// TODO: handle error
	}
}

async function selectCity(city) {
	this.city = city;
	try {
		// Query API for weather data
		const response = await fetch(`/v1/city/${city.id}`)
		this.cityData = response.ok ? await response.json() : [];
	} catch (err) {
		// TODO: handle error
	}
}

function clearCity() {
	this.city = null;
	this.cityData = null;
}
