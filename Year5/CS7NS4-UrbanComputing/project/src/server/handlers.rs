use std::collections::HashMap;
use handlebars::Handlebars;
use serde_json::json;
use axum::{http, extract::{State, Json}, response::Html};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::state;
use crate::server::users;

pub async fn get(State((state, tpl)): State<(state::SharedState, Handlebars<'static>)>) -> Html<String> {
    let state = state.lock().unwrap();

    #[derive(serde::Serialize)]
    struct User {
        username: String,
        posts: Vec<Post>,
    }

    #[derive(serde::Serialize)]
    struct Post {
        latitude: f32,
        longitude: f32,
        service_id: String,
        vehicle_id: String,
        time: String,
    }

    #[derive(serde::Serialize)]
    struct Service {
        service_id: String,
        vehicles: HashMap<String, Vehicle>,
    }

    #[derive(serde::Serialize)]
    struct Vehicle {
        vehicle_id: String,
        latitude: f32,
        longitude: f32,
        headsign: String,
        timestamp: u64,
    }

    #[derive(serde::Serialize)]
    struct Format {
        users: Vec<User>,
        services: HashMap<String, Service>,
    }

    let mut format = Format {
        users: Vec::new(),
        services: HashMap::new(),
    };

    for entity in &state.gtfsr.entity {
        if let Some(vehicle) = &entity.vehicle {
            let ts = vehicle.timestamp();
            let id = vehicle.vehicle.clone().and_then(|veh| veh.id);
            let coords = vehicle.position.clone().map(|pos| (pos.latitude, pos.longitude));
            let trip = vehicle.trip.clone().and_then(|trip| trip.trip_id).and_then(|trip_id| state.gtfs.trips.get(&trip_id));
            if let (Some(id), Some(coords), Some(trip)) = (id, coords, trip) {
                let service = format.services.entry(trip.service_id.clone()).or_insert(Service { service_id: trip.service_id.clone(), vehicles: HashMap::new() });
                service.vehicles.insert(id.clone(), Vehicle { vehicle_id: id.clone(), latitude: coords.0, longitude: coords.1, headsign: trip.headsign.clone(), timestamp: ts, });
            }
        }
    }

    for (_, user) in state.users.iter() {
        let mut posts = Vec::new();
        user.posts.iter().for_each(|(_, post)| {
            posts.push(Post {
                latitude: post.latitude,
                longitude: post.longitude,
                service_id: post.service_id.clone(),
                vehicle_id: post.vehicle_id.clone(),
                time: post.time.clone(),
            });
        });
        format.users.push(User {
            username: user.username.clone(), posts
        })
    }

    let context = json!(format);
    let html = match tpl.render("html", &context) {
        Err(e) => format!("Templating failed: {e}"),
        Ok(v) => v,
    };

    Html(html)
}


#[derive(Debug, serde::Deserialize)]
pub struct UserPost {
    username: String,
    latitude: f32,
    longitude: f32,
    service_id: String,
    vehicle_id: String,
}

pub async fn post(State(state): State<state::SharedState>, Json(input): Json<UserPost>) -> http::StatusCode {
    let mut state = state.lock().unwrap();

    let user = state.users.entry(input.username.clone()).or_insert(users::User { username: input.username, posts: HashMap::new() });
    user.posts.insert(user.posts.len().to_string(), users::Post {
        latitude: input.latitude,
        longitude: input.longitude,
        service_id: input.service_id,
        vehicle_id: input.vehicle_id,
        time: format!("{:?}", SystemTime::now().duration_since(UNIX_EPOCH).unwrap_or_default()),
    });

    http::StatusCode::OK
}
