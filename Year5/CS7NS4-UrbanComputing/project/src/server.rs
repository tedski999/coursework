use std::net::SocketAddr;
use axum::routing;
use axum_server::tls_rustls::RustlsConfig;
use handlebars::Handlebars;
use anyhow::Result;

use crate::state;

pub mod users;
pub mod handlers;

pub async fn start(state: state::SharedState, port: u16, tls: RustlsConfig, tpl: Handlebars<'static>) -> Result<()> {
    let router = axum::Router::new()
        .route("/", routing::get(handlers::get).with_state((state.clone(), tpl)))
        .route("/post", routing::post(handlers::post).with_state(state.clone()));

    let addr = SocketAddr::from(([0, 0, 0, 0], port));
    let server = axum_server::bind_rustls(addr, tls);
    server.serve(router.into_make_service()).await?;

    Ok(())
}
