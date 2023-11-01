use std::{sync::{Arc, Mutex}, collections::HashMap};

use crate::client::gtfs;
use crate::client::gtfsr;
use crate::server::users;

pub type SharedState = Arc<Mutex<State>>;

pub struct State {
    pub users: users::Users,
    pub gtfs: gtfs::Gtfs,
    pub gtfsr: gtfsr::Gtfsr,
}

pub fn new() -> SharedState {
    // TODO: load user data from file
    Arc::new(Mutex::new(State {
        users: HashMap::new(),
        gtfs: gtfs::Gtfs {
            agencies: HashMap::new(),
            stops: HashMap::new(),
            routes: HashMap::new(),
            trips: HashMap::new(),
            shapes: HashMap::new(),
            services: HashMap::new(),
        },
        gtfsr: gtfsr::Gtfsr::default()
    }))
}
