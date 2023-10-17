use std::{env, process};
use std::io::{Cursor, Write};
use std::fs::File;
use std::collections::HashMap;
use zip::ZipArchive;
use prost::Message;

pub mod gtfsr {
    include!(concat!(env!("OUT_DIR"), "/transit_realtime.rs"));
}

struct Point {
    id: String,
    service: String,
    headsign: String,
    lat: f32,
    lon: f32,
}

const KML_HEADER: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">
    <Document>
        <name>Assignment 2</name>
        <open>1</open>
        <Style id="p"><IconStyle><Icon><href>https://upload.wikimedia.org/wikipedia/commons/0/0e/Basic_red_dot.png</href></Icon><scale>0.4</scale></IconStyle></Style>
"#;
const KML_FOOTER: &str = r#"    </Document>
</kml>"#;
const CSV_HEADER: &str = r#"id,service,headsign,longitude,latitude
"#;

#[tokio::main]
async fn main() {
    let http = reqwest::Client::new();

    // User needs to provide their NTA API key to fetch GTFS-RT data
    let nta_api_key = match env::var("NTA_API_KEY") {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Unable to parse NTA_API_KEY: {}", e);
            process::exit(1);
        }
    };

    // Download latest GTFS information in zip format
    println!("Downloading GTFS zip file...");
    let request = http.get("https://www.transportforireland.ie/transitData/Data/GTFS_Realtime.zip");
    let response = match request.send().await {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Unable to request zip: {}", e);
            process::exit(1);
        }
    };
    if !reqwest::StatusCode::is_success(&response.status()) {
        eprintln!("Zip request failed: {:?}", response);
        process::exit(1);
    }

    // Parse response for returned zip data
    let bytes = match response.bytes().await {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Unable to receive zip: {}", e);
            process::exit(1);
        }
    };
    let mut bytes = Cursor::new(bytes);
    let mut zip = match ZipArchive::new(&mut bytes) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Unable to read zip: {}", e);
            process::exit(1);
        }
    };
    let trips = match zip.by_name("trips.txt") {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Unable to read trips.txt from zip: {}", e);
            process::exit(1);
        }
    };

    // Create map from trip_id to service_id and trip_headsign
    let mut trip_id_map = HashMap::new();
    let mut csv = csv::Reader::from_reader(trips);
    for line in csv.records() {
        let line = match line {
            Ok(v) => v,
            Err(e) => {
                eprintln!("Unable to parse trips.txt: {}", e);
                process::exit(1);
            }
        };
        trip_id_map.insert(line[2].to_string(), (line[1].to_string(), line[3].to_string()));
    }

    // Download latest GTFS-RT vehicle information in protobuf format
    println!("Downloading GTFS-RT protobuf data...");
    let request = http.get("https://api.nationaltransport.ie/gtfsr/v2/Vehicles").header("x-api-key", nta_api_key);
    let response = match request.send().await {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Unable to request protobuf: {}", e);
            process::exit(1);
        }
    };
    if !reqwest::StatusCode::is_success(&response.status()) {
        eprintln!("Protobuf request failed: {:?}", response);
        process::exit(1);
    }

    // Parse protobuf response for returned real-time information
    let bytes = match response.bytes().await {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Unable to receive protobuf: {}", e);
            process::exit(1);
        }
    };
    let rti = match gtfsr::FeedMessage::decode(&mut Cursor::new(bytes)) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Unable to parse protobuf: {}", e);
            process::exit(1);
        }
    };

    // Now we can do something with the rti
    // As a test, just print bus locations out as a KML file that can be visualised by Google Earth
    println!("Writing output files...");

    // Generate all the points
    let mut points = Vec::new();
    for entity in &rti.entity {
        if let Some(vehicle) = entity.vehicle.clone() {
            let unknown_characters = ("?".to_string(), "Unknown".to_string());
            let (lon,lat) = vehicle.position
                .and_then(|position| Some((position.longitude, position.latitude)))
                .unwrap_or((0.0, 0.0));
            let id = vehicle.vehicle
                .and_then(|vehicle| vehicle.id)
                .unwrap_or("?".to_string());
            let (service, headsign) = &vehicle.trip
                .and_then(|trip| trip.trip_id)
                .and_then(|trip_id| trip_id_map.get(&trip_id.to_string()))
                .unwrap_or(&unknown_characters);
            let (service, headsign) = (service.to_string(), headsign.to_string());
            points.push(Point { id, service, headsign, lon, lat });
        }
    }

    // Write KML file to ass2.kml
    let mut f = File::create("ass2.kml").expect("Unable to create ass2.kml");
    f.write_all(KML_HEADER.as_bytes()).expect("Unable to write to ass2.kml");
    for point in &points {
        f.write_all(format!("<Placemark><name>Bus #{}</name><description>[{}] {}</description><styleUrl>#p</styleUrl><Point><coordinates>{},{}</coordinates></Point></Placemark>\n", point.id, point.service, point.headsign, point.lon, point.lat).as_bytes())
            .expect("Unable to write to ass2.kml");
    }
    f.write_all(KML_FOOTER.as_bytes()).expect("Unable to write to ass2.kml");

    // Also write out ass2.csv file for assignment 2 submission
    let mut f = File::create("ass2.csv").expect("Unable to create ass2.csv");
    f.write_all(CSV_HEADER.as_bytes()).expect("Unable to write to ass2.csv");
    for point in &points {
        f.write_all(format!("{},{},\"{}\",{},{}\n", point.id, point.service, point.headsign, point.lon, point.lat).as_bytes())
            .expect("Unable to write to ass2.kml");
    }
}
