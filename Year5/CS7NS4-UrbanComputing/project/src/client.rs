use std::time::Duration;
use reqwest::Client;
use anyhow::Result;

use crate::state;

pub mod gtfs;
pub mod gtfsr;

pub async fn start(state: state::SharedState, gtfs_zip: &str, gtfs_url: &str, gtfsr_url: &str, api_key: &str) -> Result<()> {
    let http = Client::new();

    state.lock().unwrap().gtfs = gtfs::get(&http, gtfs_zip, gtfs_url).await?;

    let mut poll_interval = tokio::time::interval(Duration::from_secs(60));
    loop {
        poll_interval.tick().await;
        state.lock().unwrap().gtfsr = match gtfsr::get(&http, api_key, gtfsr_url).await {
            Err(e) => { eprintln!("Failed to fetch latest GTFS-RT data: {e}"); continue; },
            Ok(v) => v,
        };
    }
}
