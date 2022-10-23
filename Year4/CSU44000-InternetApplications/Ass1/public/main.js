import { createApp } from "https://unpkg.com/vue@3/dist/vue.esm-browser.prod.js"

const app = {
	data() {
		return {
			showCityView: false,
			// Data related to showing errors
			errors: [],
			// Data related to city search
			isSearchLoading: false,
			search: "",
			results: [],
			// Data related to city view
			isCityLoading: false,
			city: null,
			weather: [],
			forecast: [],
			days: {}
		}
	},
	methods: {
		onSearchInput: onSearchInput,
		selectCity: selectCity,
		parseData: parseData
	}
}

createApp(app).mount("#app")
document.body.style.background = "#777A44";

// Handle every keypress of the search input
async function onSearchInput() {

	// Hide the city view
	this.showCityView = false;
	document.body.style.background = "#799A80";

	// Don't bother doing empty searches
	if (this.search.length === 0) {
		this.isSearchLoading = true;
		this.results = [];
		return;
	}

	// Split search on comma into city and country
	const searchComponents = this.search.split(",", 2);
	const city = (searchComponents[0] ?? "").trim();
	const country = (searchComponents[1] ?? "").trim();

	// Query search API for results
	this.isSearchLoading = true;
	try {
		const response = await fetch(`/v1/search?city=${city}&country=${country}`);
		this.results = response.ok ? await response.json() : [];
	} catch (err) {
		this.errors.push(err);
	}
	this.isSearchLoading = false;
}

// Select a city from search, fetch city weather data and show city view
async function selectCity(city) {
	this.showCityView = true;
	this.city = city;
	this.data = null
	this.results = [];

	// Query forecast API for data
	this.isCityLoading = true;
	try {
		const response = await fetch(`/v1/forecast?lat=${city.coord.lat}&lon=${city.coord.lon}`)
		this.parseData(await response.json());
	} catch (err) {
		this.errors.push(err);
		this.showCityView = false;
	}
	this.isCityLoading = false;

	// Update background color appropriately
	if (!this.showCityView || !this.weather || !this.forecast) {
		document.body.style.background = "#b40";
	} else {
		const temperature = Math.min(Math.max(this.weather.temperature.actual, 0), 30) / 30 * 230;
		const cloud = Math.min(Math.max(this.weather.cloud, 0), 100) * 0.60 + 30;
		document.body.style.background = `hsl(${230 - temperature}, ${100 - cloud}%, 50%)`;
	}
}

// Makes use of the city, weather and pollution data received from the API
function parseData(data) {

	const days = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];

	// Set the current weather and pollution
	const weather = data.weather.current;
	const pollution = data.pollution.current.list[0].components;
	this.weather = {
		description: weather.weather[0].description.charAt(0).toUpperCase() + weather.weather[0].description.slice(1),
		temperature: { actual: weather.main.temp, feelsLike: weather.main.feels_like },
		precipitation: getPrecipitation(weather),
		cloud: weather.clouds.all,
		wind: weather.wind,
		pressure: weather.main.pressure,
		humidity: weather.main.humidity,
		pollution: { pm2_5: pollution.pm2_5, pm10: pollution.pm10 }
	}

	// Set 3h-interval weather and pollution forecasts for the next 5 days
	this.forecast = {};
	for (let i = 0; i < data.weather.forecast.list.length; i++) {
		const weather = data.weather.forecast.list[i];
		const pollution = data.pollution.forecast.list.slice(i*3, i*3+3);

		const datetime = new Date(weather.dt * 1000);
		const day = days[datetime.getDay()];
		const time = datetime.toTimeString().substring(0, 5);

		if (!(day in this.forecast))
			this.forecast[day] = [];
		this.forecast[day].push({
			time: time,
			temperature: weather.main.temp,
			precipitation: getPrecipitation(weather),
			wind: weather.wind,
			pollution: Math.max(...pollution.map(p => p.components.pm2_5))
		});
	}

	// Determine days when certain weather criteria passes
	this.days = { cold: [], mild: [], hot: [], rainy: [], pollution: [] };
	Object.entries(this.forecast).forEach(([day, intervals]) => {
		intervals.forEach(interval => {
			if (!this.days.hot.includes(day) && interval.temperature >= 25)
				this.days.hot.push(day);
			else if (!this.days.mild.includes(day) && interval.temperature >= 12 && interval.temperature < 25)
				this.days.mild.push(day);
			else if (!this.days.cold.includes(day) && interval.temperature < 12)
				this.days.cold.push(day);
			if (!this.days.rainy.includes(day) && interval.precipitation > 2)
				this.days.rainy.push(day);
			if (!this.days.pollution.includes(day) && interval.pollution > 10)
				this.days.pollution.push(day);
		})
	});

	// Convert list of days to grammatical English sentences
	Object.entries(this.days).forEach(([type, days]) => {
		if (days.length == 0) {
			this.days[type] = "";
		} else if (days.length == 1) {
			this.days[type] = days[0];
		} else {
			let sentence = days[0];
			days.slice(1, days.length - 1).forEach(day => sentence += ", " + day);
			this.days[type] = sentence + " and " + days[days.length - 1];
		}
	});
}

function getPrecipitation(weather) {
	if ("rain" in weather) {
		if ("1h" in weather.rain) {
			return weather.rain["1h"] * 3;
		} else if ("3h" in weather.rain) {
			return weather.rain["3h"];
		}
	}
	return 0;
}
