use axum_server::tls_rustls::RustlsConfig;
use handlebars::Handlebars;

mod state;
mod client;
mod server;

const SERVER_PORT: u16 = 8443;
const API_KEY_ENV: &str = "NTA_API_KEY";
const TLS_CERT: &str = "./web/cert.pem";
const TLS_KEY: &str = "./web/key.pem";
const HTML_TPL: &str = "./web/template.html";
const GTFS_ZIP: &str = "./GTFS.zip";
const GTFS_URL: &str = "https://www.transportforireland.ie/transitData/Data/GTFS_Realtime.zip";
const GTFSR_URL: &str ="https://api.nationaltransport.ie/gtfsr/v2/Vehicles";

#[tokio::main]
async fn main() {

    let api_key = match std::env::var(API_KEY_ENV) {
        Err(e) => { eprintln!("Unable to get API key from {API_KEY_ENV}: {e}"); std::process::exit(1); },
        Ok(v) => v,
    };

    let tls = match RustlsConfig::from_pem_file(TLS_CERT, TLS_KEY).await {
        Err(e) => { eprintln!("Unable to load TLS certificates: {e}"); std::process::exit(1); },
        Ok(v) => v,
    };

    let html_template = match std::fs::read_to_string(HTML_TPL) {
        Err(e) => { eprintln!("Unable to load {HTML_TPL}: {e}"); std::process::exit(1); },
        Ok(v) => v,
    };

    let mut tpl = Handlebars::new();
    match tpl.register_template_string("html", html_template) {
        Err(e) => { eprintln!("Unable to parse {HTML_TPL}: {e}"); std::process::exit(1); },
        Ok(v) => v,
    }

    let state = state::new();

    let state_clone = state.clone();
    tokio::task::spawn(async move {
        if let Err(e) = client::start(state_clone, GTFS_ZIP, GTFS_URL, GTFSR_URL, &api_key).await {
            eprintln!("Client fatal error: {e}");
            std::process::exit(1);
        }
    });

    if let Err(e) = server::start(state, SERVER_PORT, tls, tpl).await {
        eprintln!("Server fatal error: {e}");
        std::process::exit(1);
    }
}
