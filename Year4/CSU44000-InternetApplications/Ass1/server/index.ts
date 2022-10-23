import routes from "./routes";
import express from "express";

// Parse envvars for port or 80 if not given
const port: number = parseInt(process.env.SERVER_PORT ?? "") || 80;

// Create our Express server
const app: express.Application = express();

// Setup routes
app.use("/v1/", routes);
app.use(express.static("./public/"));

// Start the server listening on port
const server = app.listen(port, () => {
	console.log("[Server] Server up and listening at :%s", port);
});

// Shutdown server on SIGINT
process.on("SIGINT", () => {
	console.log("[Server] Shutting down server...");
	server.close(() => console.log("Goodbye :)"));
});
