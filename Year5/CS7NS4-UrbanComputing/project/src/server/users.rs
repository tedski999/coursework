use std::collections::HashMap;

pub type UserId = String;

pub type Users = HashMap<UserId, User>;

pub struct User {
    pub username: String,
    pub posts: HashMap<String, Post>,
}

pub struct Post {
    pub latitude: f32,
    pub longitude: f32,
    pub service_id: String,
    pub vehicle_id: String,
    pub time: String,
}
