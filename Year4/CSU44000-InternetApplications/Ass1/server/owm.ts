import http from "http";
import https from "https";

// Initialize OpenWeatherMap API variables
const owmAPIKey: string = process.env.OWM_API_KEY || console.error("OWM_API_KEY not set!") || process.exit(1);
const owmAPIURL = "https://api.openweathermap.org/data/2.5";

// Time in seconds until a cached response is invalidated
const cacheTime: number = (parseInt(process.env.CACHE_SECONDS ?? "") || 60*60) * 1000;

type Response = {
	statusCode: number,
	headers: http.IncomingHttpHeaders,
	body: string
}

type CachedResponse = {
	response: Response,
	until: number
}

const requestCache: Map<string, CachedResponse> = new Map;

function owm(uri: string): Promise<Response> {
	return new Promise((resolve, reject) => {
		const url = `${owmAPIURL}${uri}&appid=${owmAPIKey}`;
		const now = Date.now();
		// Check cache for previous valid response
		const cachedResponse: CachedResponse | undefined = requestCache.get(url);
		if (cachedResponse && cachedResponse.until > now) {
			console.log(`[OWM API ${uri}] Using cached response.`);
			resolve(cachedResponse.response);
			return;
		}
		// Otherwise, send a HTTPS request and cache the response
		console.log(`[OWM API ${uri}] URI not cached, querying OWM API...`);
		const req = https.get(url, res => {
			let body = "";
			res.on("data", chunk => body += chunk.toString());
			res.on("error", reject);
			res.on("end", () => {
				const r: Response = { statusCode: res.statusCode ?? 0, headers: res.headers, body: body };
				if (r.statusCode >= 200 && r.statusCode <= 299) {
					const valid: number = now + cacheTime;
					console.log(`[OWM API ${uri}] Response caching until`, new Date(valid));
					requestCache.set(url, { response: r, until: valid });
					resolve(r);
				} else {
					console.log(`[OWM API ${uri}] Request failed! Status code: ${res.statusCode}`);
					reject(res.statusCode);
				}
			});
		});
		req.on("error", reject);
		req.end();
	});
}

export { Response, owm };
