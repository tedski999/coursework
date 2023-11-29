use std::sync::{Arc, Mutex};
use std::collections::{HashMap, HashSet};
use std::net::SocketAddr;
use std::io::Cursor;
use std::process::exit;
use chrono::{NaiveTime, NaiveDate, Datelike};
use serde::{Serialize, Deserialize};
use tokio::time::{Duration, Instant, interval_at};
use axum_server::tls_rustls::RustlsConfig;
use axum::{extract::{State, Path, Query}, http::StatusCode, response::{Html, Json}};
use handlebars::Handlebars;
use reqwest::Client;
use zip::ZipArchive;

use prost::Message;
use gtfs::FeedMessage;

mod gtfs {
    include!(concat!(env!("OUT_DIR"), "/transit_realtime.rs"));
}

type AgencyId = String;
type StopId = String;
type RouteId = String;
type TripId = String;
type ShapeId = String;
type DaysId = String;
type VehicleId = String;
type UserId = String;

#[derive(Deserialize, Serialize)]
struct App {
    agencies: HashMap<AgencyId, Agency>,
    stops: HashMap<StopId, Stop>,
    routes: HashMap<RouteId, Route>,
    trips: HashMap<TripId, Trip>,
    shapes: HashMap<ShapeId, Vec<ShapePoint>>,
    days: HashMap<DaysId, HashSet<NaiveDate>>,
    delays: HashMap<(RouteId, TripId), HashMap<StopId, i32>>,
}

#[derive(Serialize, Deserialize)] struct Agency { name: String, url: String, tz: String }
#[derive(Serialize, Deserialize)] struct Stop { code: String, name: String, lat: f64, lon: f64 }
#[derive(Serialize, Deserialize)] struct Route { agency_id: AgencyId, short: String, long: String, category: String }
#[derive(Serialize, Deserialize)] struct Trip { route_id: RouteId, days_id: DaysId, sign: String, dir: bool, times: Vec<TripTime>, shape_id: ShapeId, vehicles: HashMap<VehicleId, (f32, f32)>, subs: HashMap<UserId, i32> }
#[derive(Serialize, Deserialize)] struct TripTime { time: NaiveTime, stop_id: StopId }
#[derive(Serialize, Deserialize)] struct ShapePoint { lat: f64, lon: f64, dist: f32 }

#[derive(Deserialize)]
struct StopsQuery {
    #[serde(rename(deserialize = "s"))] src_id: String,
    #[serde(rename(deserialize = "d"))] dst_id: String,
    #[serde(rename(deserialize = "r"))] dir: bool,
}

const SERVER_PORT: u16 = 8443;
const API_KEY_ENV: &str = "NTA_API_KEY";
const TLS_CERT: &str = "./web/cert.pem";
const TLS_KEY: &str = "./web/key.pem";
const SEARCH_TPL: &str = "./web/templates/search.html";
const ROUTE_TPL: &str = "./web/templates/route.html";
const TRIP_TPL: &str = "./web/templates/trip.html";
const STATE_FILE: &str = "./state.json";
const GTFS_URL: &str = "https://www.transportforireland.ie/transitData/Data/GTFS_Realtime.zip";
const GTFSR_UPDATES_URL: &str ="https://api.nationaltransport.ie/gtfsr/v2/TripUpdates";
const GTFSR_VEHICLES_URL: &str ="https://api.nationaltransport.ie/gtfsr/v2/Vehicles";
const GTFSR_POLL_START_DELAY: Duration = Duration::from_secs(0);
const GTFSR_POLL_INTERVAL: Duration = Duration::from_secs(60);

#[tokio::main]
async fn main() {
    let api_key = match std::env::var(API_KEY_ENV) { Err(e) => { eprintln!("Unable to get API key from {API_KEY_ENV}: {e}"); exit(1); }, Ok(v) => v };
    let tls = match RustlsConfig::from_pem_file(TLS_CERT, TLS_KEY).await { Err(e) => { eprintln!("Unable to load TLS certificates: {e}"); exit(1); }, Ok(v) => v };

    let search_tpl = match std::fs::read_to_string(SEARCH_TPL) { Err(e) => { eprintln!("Unable to load {SEARCH_TPL}: {e}"); exit(1); }, Ok(v) => v };
    let route_tpl = match std::fs::read_to_string(ROUTE_TPL) { Err(e) => { eprintln!("Unable to load {ROUTE_TPL}: {e}"); exit(1); }, Ok(v) => v };
    let trip_tpl = match std::fs::read_to_string(TRIP_TPL) { Err(e) => { eprintln!("Unable to load {TRIP_TPL}: {e}"); exit(1); }, Ok(v) => v };

    let mut tpl = Handlebars::new();
    match tpl.register_template_string("search", search_tpl) { Err(e) => { eprintln!("Unable to parse {SEARCH_TPL}: {e}"); exit(1); }, Ok(v) => v }
    match tpl.register_template_string("route", route_tpl) { Err(e) => { eprintln!("Unable to parse {ROUTE_TPL}: {e}"); exit(1); }, Ok(v) => v }
    match tpl.register_template_string("trip", trip_tpl) { Err(e) => { eprintln!("Unable to parse {TRIP_TPL}: {e}"); exit(1); }, Ok(v) => v }

    let client = Client::new();

    let app = match std::fs::read(STATE_FILE) {
        Err(_) => {
            println!("Fetching latest GTFS data...");
            let res = match client.get(GTFS_URL).send().await { Err(e) => { eprintln!("Unable to get {GTFS_URL}: {e}"); exit(1); }, Ok(v) => v };
            if !StatusCode::is_success(&res.status()) { eprintln!("Unsuccessful {GTFS_URL} response: {}", res.status()); exit(1); }
            let bytes = match res.bytes().await { Err(e) => { eprintln!("Unable to read {GTFS_URL} response: {e}"); exit(1); }, Ok(v) => v };
            let bytes = Cursor::new(bytes.to_vec());
            let mut zip = match ZipArchive::new(bytes) { Err(e) => { eprintln!("Unable to parse {GTFS_URL} response: {e}"); exit(1); }, Ok(v) => v };

            println!("Parsing GTFS data...");
            println!("  Agencies...");
            let mut agencies = HashMap::new();
            for r in csv::Reader::from_reader(match zip.by_name("agency.txt") { Err(e) => { eprintln!("Unable to read {GTFS_URL} agency.txt: {e}"); exit(1); }, Ok(v) => v }).records() {
                let r = match r { Err(e) => { eprintln!("  Ignoring bad agency: {e}"); continue; }, Ok(v) => v };
                agencies.insert(r[0].into(), Agency { name: r[1].into(), url: r[2].into(), tz: r[3].into() });
            }
            println!("  Stops...");
            let mut stops = HashMap::new();
            for r in csv::Reader::from_reader(match zip.by_name("stops.txt") { Err(e) => { eprintln!("Unable to read {GTFS_URL} stops.txt: {e}"); exit(1); }, Ok(v) => v }).records() {
                let r = match r { Err(e) => { eprintln!("  Ignoring bad stop: {e}"); continue; }, Ok(v) => v };
                stops.insert(r[0].into(), Stop { code: r[1].into(), name: r[2].into(), lat: r[4].parse().unwrap_or(0.0), lon: r[5].parse().unwrap_or(0.0) });
            }
            println!("  Routes...");
            let mut routes = HashMap::new();
            for r in csv::Reader::from_reader(match zip.by_name("routes.txt") { Err(e) => { eprintln!("Unable to read {GTFS_URL} routes.txt: {e}"); exit(1); }, Ok(v) => v }).records() {
                let r = match r { Err(e) => { eprintln!("  Ignoring bad route: {e}"); continue; }, Ok(v) => v };
                routes.insert(r[0].into(), Route { agency_id: r[1].into(), short: r[2].into(), long: r[3].into(), category: r[5].into() });
            }
            println!("  Trips...");
            let mut trips = HashMap::new();
            for r in csv::Reader::from_reader(match zip.by_name("trips.txt") { Err(e) => { eprintln!("Unable to read {GTFS_URL} trips.txt: {e}"); exit(1); }, Ok(v) => v }).records() {
                let r = match r { Err(e) => { eprintln!("  Ignoring bad trip: {e}"); continue; }, Ok(v) => v };
                trips.insert(r[2].into(), Trip { route_id: r[0].into(), days_id: r[1].into(), sign: r[3].into(), dir: r[5].eq("0"), times: Vec::new(), shape_id: r[7].into(), vehicles: HashMap::new(), subs: HashMap::new() });
            }
            for r in csv::Reader::from_reader(match zip.by_name("stop_times.txt") { Err(e) => { eprintln!("Unable to read {GTFS_URL} stop_times.txt: {e}"); exit(1); }, Ok(v) => v }).records() {
                let r = match r { Err(e) => { eprintln!("  Ignoring bad trip time: {e}"); continue; }, Ok(v) => v };
                if let Some(trip) = trips.get_mut(r[0].into()) {
                    let time = match NaiveTime::parse_from_str(&r[1], "%H:%M:%S") { Err(_) => { continue; }, Ok(v) => v };
                    trip.times.push(TripTime { time, stop_id: r[3].into() });
                } else {
                    println!("  Ignoring trip time of non-existent trip {}", r[0].to_owned());
                }
            }
            for trip in trips.values_mut() {
                trip.times.sort_by(|a, b| a.time.cmp(&b.time));
            }
            println!("  Shapes...");
            let mut shapes = HashMap::new();
            for r in csv::Reader::from_reader(match zip.by_name("shapes.txt") { Err(e) => { eprintln!("Unable to read {GTFS_URL} shapes.txt: {e}"); exit(1); }, Ok(v) => v }).records() {
                let r = match r { Err(e) => { eprintln!("  Ignoring bad shape: {e}"); continue; }, Ok(v) => v };
                let shape = shapes.entry(r[0].into()).or_insert(Vec::new());
                shape.push(ShapePoint { lat: r[1].parse().unwrap_or(0.0), lon: r[2].parse().unwrap_or(0.0), dist: r[4].parse().unwrap_or(0.0) });
            }
            println!("  Services...");
            let mut days = HashMap::new();
            for r in csv::Reader::from_reader(match zip.by_name("calendar.txt") { Err(e) => { eprintln!("Unable to read {GTFS_URL} calendar.txt: {e}"); exit(1); }, Ok(v) => v }).records() {
                let r = match r { Err(e) => { eprintln!("  Ignoring bad service: {e}"); continue; }, Ok(v) => v };
                let mut set = HashSet::new();
                let start = match NaiveDate::parse_from_str(&r[8], "%Y%m%d") { Err(e) => { eprintln!("  Ignoring bad service start format: {e}"); continue; }, Ok(v) => v };
                let end = match NaiveDate::parse_from_str(&r[9], "%Y%m%d") { Err(e) => { eprintln!("  Ignoring bad service end format: {e}"); continue; }, Ok(v) => v };
                for day in start.iter_days().take_while(|day| day.ne(&end)) {
                    if r[day.weekday().number_from_monday() as usize].eq("1") {
                        set.insert(day);
                    }
                }
                days.insert(r[0].into(), set);
            }
            for r in csv::Reader::from_reader(match zip.by_name("calendar_dates.txt") { Err(e) => { eprintln!("Unable to read {GTFS_URL} calendar_dates.txt: {e}"); exit(1); }, Ok(v) => v }).records() {
                let r = match r { Err(e) => { eprintln!("  Ignoring bad service exception: {e}"); continue; }, Ok(v) => v };
                if let Some(set) = days.get_mut(r[0].into()) {
                    let day = match NaiveDate::parse_from_str(&r[1], "%Y%m%d") { Err(e) => { eprintln!("  Ignoring bad service exception format: {e}"); continue; }, Ok(v) => v };
                    if r[2].eq("2") { set.remove(&day); } else if r[2].eq("1") { set.insert(day); }
                } else {
                    println!("  Ignoring service exception of non-existent service {}", r[0].to_owned());
                }
            }

            println!("Caching GTFS data...");
            let app = App { agencies, stops, routes, trips, shapes, days, delays: HashMap::new() };
            let bytes = match serde_json::to_string(&app) { Err(e) => { eprintln!("Unable to serialise state: {e}"); exit(1); }, Ok(v) => v };
            match std::fs::write(STATE_FILE, bytes) { Err(e) => { eprintln!("Unable to write state: {e}"); exit(1); }, Ok(v) => v };
            app
        }
        Ok(v) => {
            println!("Loading locally cached GTFS data...");
            match serde_json::from_slice(&v) { Err(e) => { eprintln!("Unable to parse {STATE_FILE}: {e}"); exit(1); }, Ok(v) => v }
        },
    };

    println!("  Agencies: {}", app.agencies.len());
    println!("  Stops: {}", app.stops.len());
    println!("  Routes: {}", app.routes.len());
    println!("  Trips: {}", app.trips.len());
    println!("  Shapes: {}", app.shapes.len());
    println!("  Services: {}", app.days.len());

    let app = Arc::new(Mutex::new(app));
    let app_clone = app.clone();

    tokio::task::spawn(async move {
        let mut poll_interval = interval_at(Instant::now() + GTFSR_POLL_START_DELAY, GTFSR_POLL_INTERVAL);
        loop {
            poll_interval.tick().await;

            println!("Fetching latest GTFS-RT vehicle data...");
            let req = client.get(GTFSR_VEHICLES_URL).header("x-api-key", &api_key);
            let res = match req.send().await { Err(e) => { eprintln!("Unable to get {GTFSR_VEHICLES_URL}: {e}"); continue; }, Ok(v) => v };
            if !StatusCode::is_success(&res.status()) { eprintln!("Unsuccessful {GTFSR_VEHICLES_URL} response: {}", res.status()); continue; }
            let bytes = match res.bytes().await { Err(e) => { eprintln!("Unable to read {GTFSR_VEHICLES_URL} response: {e}"); continue; }, Ok(v) => v };
            let msg = match FeedMessage::decode(bytes) { Err(e) => { eprintln!("Unable to parse {GTFSR_VEHICLES_URL} response: {e}"); continue; }, Ok(v) => v };
            {
                let mut app = app_clone.lock().unwrap();
                for trip in app.trips.values_mut() { trip.vehicles = HashMap::new(); }
                for e in &msg.entity {
                    if let Some(v) = &e.vehicle {
                        if let (Some(t), Some(p), Some(v)) = (&v.trip, &v.position, &v.vehicle) {
                            if let (Some(ti), Some(vi)) = (&t.trip_id, &v.id) {
                                if let Some(ts) = app.trips.get_mut(ti) {
                                    ts.vehicles.insert(vi.to_owned(), (p.latitude, p.longitude));
                                }
                            }
                        }
                    }
                }
            }
            println!("Done.");

            poll_interval.tick().await;

            println!("Fetching latest GTFS-RT trip updates data...");
            let req = client.get(GTFSR_UPDATES_URL).header("x-api-key", &api_key);
            let res = match req.send().await { Err(e) => { eprintln!("Unable to get {GTFSR_UPDATES_URL}: {e}"); continue; }, Ok(v) => v };
            if !StatusCode::is_success(&res.status()) { eprintln!("Unsuccessful {GTFSR_UPDATES_URL} response: {}", res.status()); continue; }
            let bytes = match res.bytes().await { Err(e) => { eprintln!("Unable to read {GTFSR_UPDATES_URL} response: {e}"); continue; }, Ok(v) => v };
            let msg = match FeedMessage::decode(bytes) { Err(e) => { eprintln!("Unable to parse {GTFSR_UPDATES_URL} response: {e}"); continue; }, Ok(v) => v };
            {
                let mut app = app_clone.lock().unwrap();
                app.delays = HashMap::new();
                for e in &msg.entity {
                    if let Some(t) = &e.trip_update {
                        if let (Some(ri), Some(ti)) = (&t.trip.route_id, &t.trip.trip_id) {
                            let mut max_delay = 0;
                            let values = app.delays.entry((ri.to_owned(), ti.to_owned())).or_insert_with(|| HashMap::new());
                            for u in &t.stop_time_update {
                                if let Some(id) = &u.stop_id {
                                    let a = u.arrival.clone().and_then(|a| a.delay);
                                    let b = u.departure.clone().and_then(|b| b.delay);
                                    let d = match (a, b) {
                                        (None, None) => continue,
                                        (Some(d), None) => d,
                                        (None, Some(d)) => d,
                                        (Some(a), Some(b)) => std::cmp::max(a, b),
                                    };
                                    values.insert(id.to_owned(), d);
                                    max_delay = max_delay.max(d);
                                }
                            }
                            if let Some(trip) = app.trips.get(ti) {
                                for (user_id, limit) in &trip.subs {
                                    if max_delay > *limit {
                                        println!("User {user_id} delay limit exceeded for {ti}! A notification should be sent.");
                                    }
                                }
                            }
                        }
                    }
                }
            }
            println!("Done.");
        }
    });

    let router = axum::Router::new()
        .route("/", axum::routing::get(get_search_page).with_state((app.clone(), tpl.clone())))
        .route("/:route", axum::routing::get(get_route_page).with_state((app.clone(), tpl.clone())))
        .route("/:route/:trip", axum::routing::get(get_trip_page).with_state((app.clone(), tpl.clone())))
        .route("/api/stops/:route", axum::routing::get(get_src_stops).with_state(app.clone()))
        .route("/api/stops/:route/:src", axum::routing::get(get_dst_stops).with_state(app.clone()))
        .route("/api/info/:route", axum::routing::get(get_info).with_state(app.clone()))
        .route("/api/user/:route/:trip", axum::routing::post(post_user_location).with_state(app.clone()))
        .route("/api/user/:trip/:user/:limit", axum::routing::post(post_user_subscription).with_state(app.clone()));

    println!("Server listening on :{SERVER_PORT}");
    let server = axum_server::bind_rustls(SocketAddr::from(([0, 0, 0, 0], SERVER_PORT)), tls).serve(router.into_make_service());
    if let Err(e) = server.await { eprintln!("Server fatal error: {e}"); exit(1); }
}

async fn get_search_page(State((app, tpl)): State<(Arc<Mutex<App>>, Handlebars<'static>)>) -> Html<String> {
    #[derive(Serialize)] struct Ctx { routes: Vec<CtxRoute> }
    #[derive(Serialize)] struct CtxRoute { id: String, short: String, long: String }

    let app = app.lock().unwrap();
    let mut routes = Vec::new();
    for (id, route) in &app.routes {
        routes.push(CtxRoute { id: id.to_owned(), short: route.short.to_owned(), long: route.long.to_owned() })
    }
    let ctx = Ctx { routes };

    Html(match tpl.render("search", &serde_json::json!(ctx)) { Err(e) => format!("Templating failed: {e}"), Ok(v) => v })
}

async fn get_route_page(State((app, tpl)): State<(Arc<Mutex<App>>, Handlebars<'static>)>, Path(route_id): Path<String>, Query(StopsQuery { src_id, dst_id, dir }): Query<StopsQuery>) -> Html<String> {
    #[derive(Serialize)] struct Ctx { route: CtxRoute, trips: Vec<CtxTrip>, src: CtxStop, dst: CtxStop, dir: bool, shapes: Vec<Vec<(f64, f64)>> }
    #[derive(Serialize)] struct CtxRoute { id: String, short: String, long: String }
    #[derive(Serialize)] struct CtxTrip { id: String, sign: String, departs: chrono::NaiveTime, arrives: chrono::NaiveTime, stops: u32, delay: Option<i32>, vehicle: Option<(VehicleId, f32, f32)> }
    #[derive(Serialize)] struct CtxStop { id: String, name: String, lat: f64, lon: f64 }

    let app = app.lock().unwrap();
    let src = match app.stops.get(&src_id) { None => { return Html("404".into()) }, Some(v) => v };
    let src = CtxStop { id: src_id.to_owned(), name: src.name.to_owned(), lat: src.lat, lon: src.lon };
    let dst = match app.stops.get(&dst_id) { None => { return Html("404".into()) }, Some(v) => v };
    let dst = CtxStop { id: dst_id.to_owned(), name: dst.name.to_owned(), lat: dst.lat, lon: dst.lon };
    let route = match app.routes.get(&route_id) { None => { return Html("404".into()) }, Some(v) => v };
    let route = CtxRoute { id: route_id.to_owned(), short: route.short.to_owned(), long: route.long.to_owned() };

    let mut trips = Vec::new();
    let mut shape_ids = HashSet::new();
    let date = chrono::Utc::now().date_naive();
    for (id, trip) in &app.trips {
        if trip.route_id != route_id || trip.dir != dir { continue; }
        if !app.days[&trip.days_id].contains(&date) { continue; }
        if let (Some(src_idx), Some(dst_idx)) = (trip.times.iter().position(|t| t.stop_id == src_id), trip.times.iter().position(|t| t.stop_id == dst_id)) {
            if src_idx >= dst_idx { continue; }
            shape_ids.insert(trip.shape_id.to_owned());
            trips.push(CtxTrip {
                id: id.to_owned(),
                sign: trip.sign.to_owned(),
                departs: trip.times[src_idx].time,
                arrives: trip.times[dst_idx].time,
                stops: (dst_idx - src_idx) as u32,
                vehicle: if let Some((id, (lat, lon))) = trip.vehicles.iter().next() { Some((id.to_owned(), *lat, *lon)) } else { None },
                delay: match app.delays.get(&(route_id.to_owned(), id.to_owned())) {
                    None => None,
                    Some(d) => {
                        match d.get(&src.id) {
                            None => if d.len() == 0 { None } else { Some(d.values().sum::<i32>() / d.len() as i32) },
                            Some(d) => Some(*d),
                        }
                    }
                },
            });
        }
    }
    trips.sort_by(|a, b| a.departs.cmp(&b.departs));

    let mut shapes = Vec::new();
    for shape_id in &shape_ids {
        let mut shape = Vec::new();
        for p in &app.shapes[shape_id] {
            shape.push((p.lat, p.lon));
        }
        shapes.push(shape);
    }

    let ctx = Ctx { route, trips, src, dst, dir, shapes };
    Html(match tpl.render("route", &serde_json::json!(ctx)) { Err(e) => format!("Templating failed: {e}"), Ok(v) => v })
}

async fn get_trip_page(State((app, tpl)): State<(Arc<Mutex<App>>, Handlebars<'static>)>, Path((route_id, trip_id)): Path<(String, String)>, Query(StopsQuery { src_id, dst_id, dir }): Query<StopsQuery>) -> Html<String> {
    #[derive(Serialize)] struct Ctx { route: CtxRoute, trip: CtxTrip, stops: Vec<CtxStop>, dir: bool, src: CtxStop, dst: CtxStop, shape: Vec<(f64, f64)> }
    #[derive(Serialize)] struct CtxRoute { id: String, short: String, long: String }
    #[derive(Serialize)] struct CtxTrip { id: String, sign: String, delay: u32, vehicle: Option<(VehicleId, f32, f32)> }
    #[derive(Serialize)] struct CtxStop { id: String, name: String, lat: f64, lon: f64, time: chrono::NaiveTime, delay: Option<i32> }

    let app = app.lock().unwrap();
    let trip = match app.trips.get(&trip_id) { None => { return Html("404".into()) }, Some(v) => v };
    let mut shape = Vec::new(); for p in &app.shapes[&trip.shape_id] { shape.push((p.lat, p.lon)); }
    let src = match app.stops.get(&src_id) { None => { return Html("404".into()) }, Some(v) => v };
    let src = CtxStop { id: src_id.to_owned(), name: src.name.to_owned(), lat: src.lat, lon: src.lon, time: trip.times[0].time, delay: None };
    let dst = match app.stops.get(&dst_id) { None => { return Html("404".into()) }, Some(v) => v };
    let dst = CtxStop { id: dst_id.to_owned(), name: dst.name.to_owned(), lat: dst.lat, lon: dst.lon, time: trip.times[0].time, delay: None };
    let vehicles = &trip.vehicles;
    let mut trip = CtxTrip { id: trip_id.to_owned(), sign: trip.sign.to_owned(), delay: 0, vehicle: None };
    if let Some((id, (lat, lon))) = vehicles.iter().next() { trip.vehicle = Some((id.to_owned(), *lat, *lon)); }
    let route = match app.routes.get(&route_id) { None => { return Html("404".into()) }, Some(v) => v };
    let route = CtxRoute { id: route_id.to_owned(), short: route.short.to_owned(), long: route.long.to_owned() };
    let delays = app.delays.get(&(route_id.to_owned(), trip.id.to_owned()));

    let mut stops = Vec::new();
    for time in &app.trips[&trip.id].times {
        let stop = &app.stops[&time.stop_id];
        stops.push(CtxStop {
            id: time.stop_id.to_owned(),
            name: stop.name.to_owned(),
            lat: stop.lat,
            lon: stop.lon,
            time: time.time,
            delay: match delays {
                None => None,
                Some(d) => match d.get(&time.stop_id) {
                    None => None,
                    Some(d) => Some(*d),
                }
            }
        });
    }
    stops.sort_by(|a, b| a.time.cmp(&b.time));

    let mut last_delay = None;
    for stop in &mut stops {
        if let Some(delay) = stop.delay {
            last_delay = Some(delay);
        } else {
            stop.delay = last_delay;
        }
    }

    let ctx = Ctx { route, trip, stops, dir, src, dst, shape };
    Html(match tpl.render("trip", &serde_json::json!(ctx)) { Err(e) => format!("Templating failed: {e}"), Ok(v) => v })
}

#[derive(Eq, PartialEq, Hash, Serialize)] struct SrcStopsRes { id: String, name: String, dir: bool }
async fn get_src_stops(State(app): State<Arc<Mutex<App>>>, Path(route_id): Path<String>) -> Json<HashSet<SrcStopsRes>> {
    let app = app.lock().unwrap();
    let mut set = HashSet::new();
    for trip in app.trips.values().filter(|t| t.route_id.eq(&route_id)) {
        for time in &trip.times {
            let stop = &app.stops[&time.stop_id];
            set.insert(SrcStopsRes {
                id: time.stop_id.to_owned(),
                name: stop.name.to_owned(),
                dir: trip.dir
            });
        }
    }
    Json(set)
}

#[derive(Eq, PartialEq, Hash, Serialize)] struct DstStopsRes { id: String, name: String }
async fn get_dst_stops(State(app): State<Arc<Mutex<App>>>, Path((route_id, src_id)): Path<(String, String)>) -> Json<HashSet<DstStopsRes>> {
    let app = app.lock().unwrap();
    let mut set = HashSet::new();
    for trip in app.trips.values().filter(|t| t.route_id.eq(&route_id)) {
        if let Some(idx) = trip.times.iter().position(|t| t.stop_id.eq(&src_id)) {
            for time in &trip.times[idx+1..] {
                let stop = &app.stops[&time.stop_id];
                set.insert(DstStopsRes {
                    id: time.stop_id.to_owned(),
                    name: stop.name.to_owned(),
                });
            }
        }
    }
    Json(set)
}

#[derive(Eq, PartialEq, Hash, Serialize)] struct InfoRes { agency: String, category: String, trips: u32, next: Option<(NaiveTime, Option<i32>)> }
async fn get_info(State(app): State<Arc<Mutex<App>>>, Path(route_id): Path<String>, Query(StopsQuery { src_id, dst_id, dir }): Query<StopsQuery>) -> Json<InfoRes> {
    let mut res = InfoRes { agency: "".into(), category: "".into(), trips: 0, next: None };

    let app = app.lock().unwrap();
    if !app.stops.contains_key(&src_id) { return Json(res); }
    if !app.stops.contains_key(&dst_id) { return Json(res); }
    let route = match app.routes.get(&route_id) { None => return Json(res), Some(v) => v };
    res.agency = app.agencies[&route.agency_id].name.to_owned();
    res.category = route.category.to_owned();

    let empty_map = HashMap::new();

    let now = chrono::Utc::now();
    let time = now.time();
    let day = now.date_naive();
    for (id, trip) in &app.trips {
        if trip.route_id != route_id || trip.dir != dir { continue; }
        if !app.days[&trip.days_id].contains(&day) { continue; }
        if let (Some(src_idx), Some(dst_idx)) = (trip.times.iter().position(|t| t.stop_id == src_id), trip.times.iter().position(|t| t.stop_id == dst_id)) {
            if src_idx >= dst_idx { continue; }
            res.trips += 1;
            let stime = trip.times[src_idx].time;
            if stime > time && (res.next.is_none() || stime < res.next.unwrap().0) {
                let d = app.delays.get(&(route_id.to_owned(), id.to_owned())).unwrap_or(&empty_map);
                res.next = Some((stime, if d.len() == 0 { None } else { Some(d.values().sum::<i32>() / d.len() as i32) }));
            }
        }
    }

    Json(res)
}

async fn post_user_location(State(app): State<Arc<Mutex<App>>>, Path((trip_id, vehicle_id)): Path<(String, String)>, Json(coords): Json<(f64, f64)>) -> StatusCode {
    println!("New GPS reading from user for vehicle {vehicle_id} on trip {trip_id}: ({}, {})", coords.0, coords.1);
    let mut app = app.lock().unwrap();
    let trip = match app.trips.get(&trip_id) { None => { return StatusCode::NOT_FOUND }, Some(v) => v };
    let coords = snap_to_shape(coords, &app.shapes[&trip.shape_id]);
    println!("Snapped GPS reading to nearest point on trip shape: ({}, {})", coords.0, coords.1);
    let trip = match app.trips.get_mut(&trip_id) { None => { return StatusCode::NOT_FOUND }, Some(v) => v };
    trip.vehicles.insert(vehicle_id, (coords.0 as f32, coords.1 as f32));
    StatusCode::OK
}

async fn post_user_subscription(State(app): State<Arc<Mutex<App>>>, Path((trip_id, user_id, limit)): Path<(String, String, u16)>) -> StatusCode {
    println!("New user subscription by {user_id} on trip {trip_id} exceeding {limit}s delay.");
    let mut app = app.lock().unwrap();
    let trip = match app.trips.get_mut(&trip_id) { None => { return StatusCode::NOT_FOUND }, Some(v) => v };
    trip.subs.insert(user_id, limit.into());
    StatusCode::OK
}

fn snap_to_shape((lat, lon): (f64, f64), shape: &Vec<ShapePoint>) -> (f64, f64) {
    let mut min_dist = f64::INFINITY;
    let mut best_point = (lat, lon);
    for point in shape {
        let dist2 = ((lat - point.lat) * (lat - point.lat)) + ((lon - point.lon) * (lon - point.lon));
        if dist2 < min_dist {
            min_dist = dist2;
            best_point = (point.lat, point.lon);
        }
    }

    return best_point;
}
