use std::fs;
use std::io;
use std::collections::HashMap;
use reqwest::{Client, StatusCode};
use zip::ZipArchive;
use bitflags::bitflags;
use anyhow::{Result, bail};

pub type AgencyId = String;
pub type StopId = String;
pub type RouteId = String;
pub type TripId = String;
pub type ShapeId = String;
pub type ServiceId = String;

pub struct Gtfs {
    pub agencies: HashMap<AgencyId, Agency>,
    pub stops: HashMap<StopId, Stop>,
    pub routes: HashMap<RouteId, Route>,
    pub trips: HashMap<TripId, Trip>,
    pub shapes: HashMap<ShapeId, Shape>,
    pub services: HashMap<ServiceId, Service>,
}

pub struct Agency {
    pub name: String,
    pub url: String,
    pub timezone: String,
}

pub struct Stop {
    pub code: String,
    pub name: String,
    pub lat: f64,
    pub lon: f64,
}

pub struct Route {
    pub agency_id: AgencyId,
    pub short_name: String,
    pub long_name: String,
    pub route_type: String,
}

pub struct Trip {
    pub route_id: RouteId,
    pub service_id: ServiceId,
    pub headsign: String,
    pub short_name: String,
    pub direction: bool,
    pub stop_times: Vec<TripStopTime>,
    pub shape_id: ShapeId,
}

pub struct TripStopTime {
    pub time: String,
    pub stop_id: StopId,
    pub pickup_type: String,
    pub dropoff_type: String,
}

pub struct Shape {
    pub points: Vec<ShapePoint>,
}

pub struct ShapePoint {
    pub lat: f64,
    pub lon: f64,
    pub dist: f32,
}

pub struct Service {
    pub days: DaysOfWeek,
    pub start_date: String,
    pub end_date: String,
    pub exceptions: HashMap<String, String>,
}

bitflags! {
    pub struct DaysOfWeek: u8 {
        const MONDAY    = 1 << 0;
        const TUESDAY   = 1 << 1;
        const WEDNESDAY = 1 << 2;
        const THURSDAY  = 1 << 3;
        const FRIDAY    = 1 << 4;
        const SATURDAY  = 1 << 5;
        const SUNDAY    = 1 << 6;
    }
}

pub async fn get(http: &Client, file: &str, url: &str) -> Result<Gtfs> {
    println!("Fetching GTFS data...");
    let bytes = match fs::read(file) {
        Err(_) => write_zip(&fetch_zip(http, url).await?, file)?,
        Ok(v) => { println!("  Using locally cached copy."); v },
    };
    println!("Parsing GTFS data...");
    let gtfs = parse_zip(&bytes);
    println!("Done.");
    gtfs
}

async fn fetch_zip(http: &Client, url: &str) -> Result<Vec<u8>> {
    println!("  Downloading...");
    let res = http.get(url).send().await?;
    if !StatusCode::is_success(&res.status()) { bail!("Server responded {}", res.status()); }
    Ok(res.bytes().await?.to_vec())
}

fn write_zip(bytes: &[u8], file: &str) -> Result<Vec<u8>> {
    println!("  Writing...");
    fs::write(file, bytes)?;
    Ok(bytes.to_vec())
}

fn parse_zip(bytes: &[u8]) -> Result<Gtfs> {
    let mut zip = ZipArchive::new(io::Cursor::new(bytes))?;
    let agencies = extract_agencies(&mut zip)?;
    let stops = extract_stops(&mut zip)?;
    let routes = extract_routes(&mut zip)?;
    let trips = extract_trips(&mut zip)?;
    let shapes = extract_shapes(&mut zip)?;
    let services = extract_services(&mut zip)?;
    Ok(Gtfs { agencies, stops, routes, trips, shapes, services })
}

fn extract_agencies(zip: &mut ZipArchive<io::Cursor<&[u8]>>) -> Result<HashMap<AgencyId, Agency>> {
    println!("  Agencies...");
    let mut agencies = HashMap::new();
    for line in csv::Reader::from_reader(zip.by_name("agency.txt")?).records() {
        let line = line?;
        let id = line[0].to_string();
        let name = line[1].to_string();
        let url = line[2].to_string();
        let timezone = line[3].to_string();
        agencies.insert(id, Agency { name, url, timezone });
    }
    Ok(agencies)
}

fn extract_stops(zip: &mut ZipArchive<io::Cursor<&[u8]>>) -> Result<HashMap<StopId, Stop>> {
    println!("  Stops...");
    let mut stops = HashMap::new();
    for line in csv::Reader::from_reader(zip.by_name("stops.txt")?).records() {
        let line = line?;
        let id = line[0].to_string();
        let code = line[1].to_string();
        let name = line[2].to_string();
        let lat = line[4].parse()?;
        let lon = line[5].parse()?;
        stops.insert(id, Stop { code, name, lat, lon });
    }
    Ok(stops)
}

fn extract_routes(zip: &mut ZipArchive<io::Cursor<&[u8]>>) -> Result<HashMap<RouteId, Route>> {
    let mut routes = HashMap::new();
    for line in csv::Reader::from_reader(zip.by_name("routes.txt")?).records() {
        let line = line?;
        let id = line[0].to_string();
        let agency_id = line[1].to_string();
        let short_name = line[2].to_string();
        let long_name = line[3].to_string();
        let route_type = line[5].to_string();
        routes.insert(id, Route { agency_id, short_name, long_name, route_type });
    }
    Ok(routes)
}

fn extract_trips(zip: &mut ZipArchive<io::Cursor<&[u8]>>) -> Result<HashMap<TripId, Trip>> {
    println!("  Trips...");
    let mut trips = HashMap::new();
    for line in csv::Reader::from_reader(zip.by_name("trips.txt")?).records() {
        let line = line?;
        let id = line[2].to_string();
        let route_id = line[0].to_string();
        let service_id = line[1].to_string();
        let headsign = line[3].to_string();
        let short_name = line[4].to_string();
        let direction = line[5].to_string() == "0";
        let stop_times = Vec::new();
        let shape_id = line[7].to_string();
        let trip = Trip { route_id, service_id, headsign, short_name, direction, stop_times, shape_id };
        trips.insert(id, trip);
    }
    for line in csv::Reader::from_reader(zip.by_name("stop_times.txt")?).records() {
        let line = line?;
        let id = line[0].to_string();
        if let Some(trip) = trips.get_mut(&id) {
            let time = line[1].to_string();
            let stop_id = line[3].to_string();
            let pickup_type = line[6].to_string();
            let dropoff_type = line[7].to_string();
            let stop_time = TripStopTime { time, stop_id, pickup_type, dropoff_type };
            trip.stop_times.push(stop_time);
        }
    }
    Ok(trips)
}

fn extract_shapes(zip: &mut ZipArchive<io::Cursor<&[u8]>>) -> Result<HashMap<ShapeId, Shape>> {
    println!("  Shapes...");
    let mut shapes = HashMap::new();
    for line in csv::Reader::from_reader(zip.by_name("shapes.txt")?).records() {
        let line = line?;
        let id = line[0].to_string();
        let shape = shapes.entry(id).or_insert(Shape { points: Vec::new() });
        let lat = line[1].parse()?;
        let lon = line[2].parse()?;
        let dist = line[4].parse()?;
        let point = ShapePoint { lat, lon, dist };
        shape.points.push(point);
    }
    Ok(shapes)
}

fn extract_services(zip: &mut ZipArchive<io::Cursor<&[u8]>>) -> Result<HashMap<ServiceId, Service>> {
    println!("  Services...");
    let mut services = HashMap::new();
    for line in csv::Reader::from_reader(zip.by_name("calendar.txt")?).records() {
        let line = line?;
        let id = line[0].to_string();
        let mut days = DaysOfWeek::empty();
        if line[1].to_string() == "0" { days = days.union(DaysOfWeek::MONDAY); }
        if line[2].to_string() == "0" { days = days.union(DaysOfWeek::TUESDAY); }
        if line[3].to_string() == "0" { days = days.union(DaysOfWeek::WEDNESDAY); }
        if line[4].to_string() == "0" { days = days.union(DaysOfWeek::THURSDAY); }
        if line[5].to_string() == "0" { days = days.union(DaysOfWeek::FRIDAY); }
        if line[6].to_string() == "0" { days = days.union(DaysOfWeek::SATURDAY); }
        if line[7].to_string() == "0" { days = days.union(DaysOfWeek::SUNDAY); }
        let start_date = line[8].to_string();
        let end_date = line[9].to_string();
        let service = Service { days, start_date, end_date, exceptions: HashMap::new() };
        services.insert(id, service);
    }
    for line in csv::Reader::from_reader(zip.by_name("calendar_dates.txt")?).records() {
        let line = line?;
        let id = line[0].to_string();
        if let Some(service) = services.get_mut(&id) {
            let time = line[1].to_string();
            let exception = line[2].to_string();
            service.exceptions.insert(time, exception);
        }
    }
    Ok(services)
}
