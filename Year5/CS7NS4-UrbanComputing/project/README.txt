CS7NS4 Urban Computing - Assignment 2 - Sensor Data Collection
Ted Johnson, TCD 19335618, M.Sc. CS - 17th of October 2023

============
INTRODUCTION
============

This project demonstrates how the Automatic Vehicle Location (AVL) sensor data presently collected on Dublin Bus, Bus Éireann and Go-Ahead Ireland vehicles can be retrieved and parsed from the General Transit Feed Specification (GTFS) Realtime (GTFS-R) open feed provided by the National Transport Authority (NTA). This project also makes use of additional GTFS data containing information such as bus timetables and all bus stop locations. These are the same data sources used to implement many of the live passenger services we see in Ireland today, such as the Dublin Bus app bus tracking feature and the Real-Time Passenger Information (RTPI) displays found at many bus stops throughout the country.

I also plan on collecting train arrivals and departures information from the Irish Rail Realtime APIs as well as user submitted reports of individual vehicle delays. From these data sources, I hope to develop an automated online service capable of notifying its users if a vehicle they plan to or are currently riding is running late, determining if it could impact their transport connections and suggesting alternative routes to reach their destination based on live information.

====================
BUILDING AND RUNNING
====================

To build and run the application, you will first need to have both Cargo and a Protocol Buffers compiler installed. You will also need to be in possession of an NTA Developer API key, which are freely available at https://developer.nationaltransport.ie. The binary can be built with `cargo build` and run with `NTA_API_KEY=<Your API key> cargo run`. This will request and parse the appropriate resources from the NTA API and then produce two files: ass2.csv and ass2.kml. Both of these files contain the vehicle ID, bus service number, bus headsign current longitude+latitude of all reporting vehicles. It is possible to visualise this data by importing the ass2.kml file to any KML viewer, such as Google Earth Pro. The ass2.csv file only presents this information in CSV format and was submitted alongside this project as required.
