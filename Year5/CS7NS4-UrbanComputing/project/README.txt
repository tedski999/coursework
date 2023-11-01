CS7NS4 Urban Computing - Assignment 3 - Sensor Data Collection
Ted Johnson, TCD 19335618, M.Sc. CS - 31th of October 2023

============
INTRODUCTION
============

This assignment submission builds on top of the work done in Assessment 2. I have continued to develop an automated online system capable of notifying users if a public transport vehicle they currently or plan to ride is running late, determining if it could impact their planned transport connections and suggesting alternative routes to reach their destination based on live information. For this assignment, I have extended this project design to additionally accept user submitted bus locations using their mobile device's GPS readings as an alternative to the Automatic Vehicle Location (AVL) sensor data collection already implemented in the previous submission.

I must note that my work for Assignment 2 primarily involved gathering of data from an Open Data source, specifically from the AVL API provided by the National Transport Authority, as I had not planned to collect user sensor data. However, as the project developed, it soon became apparent that collecting device GPS data from participating users would be of great benefit. As such, for Task 2 of this submission, I have actually focused on what should have been Assignment 2 Task 1 “Collecting Data”. You will find that the work I did for Assignment 2 Task 1 should actually serve for what is required of Assignment 3 Task 2. I apologise if this is confusion and possible inconvenience. I am happy to reorder the sections and resubmit my two reports in a fashion that better follows the order of the assignment specifications, if that can be permitted.

====================
BUILDING AND RUNNING
====================

To build and run the application, you will first need to have both Cargo and a Protocol Buffers compiler installed. You will also need to be in possession of an NTA Developer API key, which are freely available at https://developer.nationaltransport.ie. The binary can be built with `cargo build` and run with `NTA_API_KEY=<Your API key> cargo run`. This will download and preprocess the required resources and start a TLS web server on port :8443. You may visit this site to view the gathered data and to submit your own sensor readings, provided you bypass the self-signed TLS certificate warning.
