use reqwest::{Client, StatusCode};
use anyhow::{Result, bail};
use prost::Message;

include!(concat!(env!("OUT_DIR"), "/transit_realtime.rs"));

pub type Gtfsr = FeedMessage;

pub async fn get(http: &Client, key: &str, url: &str) -> Result<FeedMessage> {
    println!("Fetching latest GTFS-RT data...");
    let res = http.get(url).header("x-api-key", key).send().await?;
    if !StatusCode::is_success(&res.status()) { bail!("Server responded {}", res.status()); }
    println!("Parsing latest GTFS-RT data...");
    let gtfsr = FeedMessage::decode(res.bytes().await?)?;
    println!("Done.");
    Ok(gtfsr)
}
