import { City, citySearch } from "./cities";
import { Response, owm } from "./owm";
import express from "express";

// Create the Express subrouter for handling API requests
const router: express.Router = express.Router();

// Responds to queries with a list of cities with matching prefix
router.get("/search", async (req: express.Request, res: express.Response) => {
	if (req.query.city === undefined || req.query.country === undefined) {
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
router.get("/city/:id", async (req: express.Request, res: express.Response) => {
	// Parse request
	const cityID: string = req.params.id;
	try {
		// Query OpenWeatherMap API
		const apires: Response = await owm(`/weather?id=${cityID}`);
		const json: string = JSON.parse(apires.body);
		res.status(apires.statusCode).json(json);
	} catch (err) {
		// If anything fails, blame the client
		res.sendStatus(400);
	}
})

export default router;
