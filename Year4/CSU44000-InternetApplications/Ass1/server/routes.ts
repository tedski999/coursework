import { City, citySearch } from "./cities";
import { Response, owm } from "./owm";
import express from "express";

// Create the Express subrouter for handling API requests
const router: express.Router = express.Router();

// Responds to queries with a list of cities with matching prefix
router.get("/search", async (req: express.Request, res: express.Response) => {
	if (req.query.city === undefined || req.query.city == "" || req.query.country === undefined) {
		res.sendStatus(400);
		return
	}
	// Parse queries
	const prefix: string = String(req.query.city).toLowerCase();
	const country: string = String(req.query.country).toLowerCase();
	// Search for cities
	const results: City[] = citySearch(prefix, country);
	results.length
		? res.json(results)
		: res.sendStatus(404);
})

// Responds to queries with a list of cities with matching prefix
router.get("/forecast", async (req: express.Request, res: express.Response) => {
	if (req.query.lat === undefined || req.query.lon === undefined) {
		res.sendStatus(400);
		return
	}
	// Parse queries
	const lat: string = String(req.query.lat).toLowerCase();
	const lon: string = String(req.query.lon).toLowerCase();
	try {
		// Query OpenWeatherMap API
		const responses: Response[] = await Promise.all([
			owm(`/weather?lat=${lat}&lon=${lon}&units=metric`),
			owm(`/forecast?lat=${lat}&lon=${lon}&units=metric`),
			owm(`/air_pollution?lat=${lat}&lon=${lon}`),
			owm(`/air_pollution/forecast?lat=${lat}&lon=${lon}`)
		]);
		// Send response
		res.json({
			weather: {
				current: JSON.parse(responses[0].body),
				forecast: JSON.parse(responses[1].body)
			},
			pollution: {
				current: JSON.parse(responses[2].body),
				forecast: JSON.parse(responses[3].body)
			}
		});
	} catch (err) {
		// If anything goes wrong, blame the client
		res.sendStatus(400);
	}
})

export default router;
