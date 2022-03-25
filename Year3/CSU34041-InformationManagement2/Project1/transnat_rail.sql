
--
-- Create database
--

CREATE DATABASE transnat_rail;
USE transnat_rail;


--
-- Create tables
--

CREATE TABLE bookings (
	route_id INT UNSIGNED NOT NULL,
	trip_number INT UNSIGNED NOT NULL,
	coach VARCHAR(255) NOT NULL,
	seat SMALLINT UNSIGNED NOT NULL,
	name VARCHAR(255),
	phone_number VARCHAR(255),
	email VARCHAR(320),
	PRIMARY KEY (route_id, trip_number, coach, seat)
);

CREATE TABLE trips (
	route_id INT UNSIGNED NOT NULL,
	trip_number INT UNSIGNED NOT NULL,
	locomotive_serial VARCHAR(255) NOT NULL,
	start_time TIME NOT NULL DEFAULT "08:00",
	PRIMARY KEY (route_id, trip_number)
);

CREATE TABLE routes (
	id INT UNSIGNED UNIQUE NOT NULL AUTO_INCREMENT,
	name VARCHAR(255),
	PRIMARY KEY (id)
);

CREATE TABLE visits (
	route_id INT UNSIGNED NOT NULL,
	stop_number INT UNSIGNED NOT NULL,
	station_code VARCHAR(255) NOT NULL,
	platform SMALLINT NOT NULL DEFAULT 1,
	fare DECIMAL(10,2) NOT NULL DEFAULT 2.50,
	arrival_time TIME,
	departure_time TIME,
	PRIMARY KEY (route_id, stop_number)
);

CREATE TABLE stations (
	code VARCHAR(255) UNIQUE NOT NULL,
	road VARCHAR(255),
	city VARCHAR(255),
	name VARCHAR(255),
	PRIMARY KEY (code)
);

CREATE TABLE employees (
	id INT UNSIGNED UNIQUE NOT NULL AUTO_INCREMENT,
	name VARCHAR(255) NOT NULL,
	phone_number VARCHAR(255),
	email VARCHAR(320),
	wage DECIMAL(10,2) NOT NULL,
	working_hours TIME NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE cashiers (
	employee_id INT UNSIGNED UNIQUE NOT NULL,
	station_code VARCHAR(255),
	register INT UNSIGNED,
	PRIMARY KEY (employee_id)
);

CREATE TABLE drivers (
	employee_id INT UNSIGNED UNIQUE NOT NULL,
	locomotive_serial VARCHAR(255) UNIQUE,
	PRIMARY KEY (employee_id)
);

CREATE TABLE locomotives (
	serial VARCHAR(255) UNIQUE NOT NULL,
	model VARCHAR(255),
	build_date DATETIME,
	colour VARCHAR(255),
	PRIMARY KEY (serial)
);


-- Add foreign keys
ALTER TABLE bookings ADD FOREIGN KEY (route_id, trip_number) REFERENCES trips(route_id, trip_number);
ALTER TABLE trips ADD FOREIGN KEY (route_id) REFERENCES routes(id);
ALTER TABLE trips ADD FOREIGN KEY (locomotive_serial) REFERENCES locomotives(serial);
ALTER TABLE visits ADD FOREIGN KEY (route_id) REFERENCES routes(id);
ALTER TABLE visits ADD FOREIGN KEY (station_code) REFERENCES stations(code);
ALTER TABLE cashiers ADD FOREIGN KEY (employee_id) REFERENCES employees(id);
ALTER TABLE cashiers ADD FOREIGN KEY (station_code) REFERENCES stations(code);
ALTER TABLE drivers ADD FOREIGN KEY (employee_id) REFERENCES employees(id);
ALTER TABLE drivers ADD FOREIGN KEY (locomotive_serial) REFERENCES locomotives(serial);


--
-- Create useful administrative views
--

-- Times when locomotives are active
CREATE VIEW locomotive_working_times AS
SELECT DISTINCT l.serial, t.route_id, t.trip_number, t.start_time, ADDTIME(t.start_time, v.arrival_time) as "end_time"
FROM locomotives l
	LEFT JOIN trips t ON t.locomotive_serial = l.serial
	JOIN visits v USING (route_id)
WHERE
	v.departure_time IS NULL
ORDER BY l.serial;

-- Number of hours locomotives are active
CREATE VIEW locomotive_working_hours AS
SELECT DISTINCT l.serial, SEC_TO_TIME(SUM(TIME_TO_SEC(v.arrival_time))) as "working_hours"
FROM locomotive_working_times l
	JOIN visits v USING (route_id)
WHERE
	v.departure_time IS NULL
GROUP BY l.serial
ORDER BY l.serial;

-- Locomotives assigned trips
CREATE VIEW active_locomotives AS
SELECT DISTINCT l.serial
FROM locomotive_working_times l
ORDER BY l.serial;

-- Locomotives not assigned trips
CREATE VIEW inactive_locomotives AS
SELECT DISTINCT l.serial
FROM locomotives l
	LEFT JOIN active_locomotives a USING (serial)
WHERE a.serial IS NULL
ORDER BY l.serial;

-- Drivers without active locomotives
CREATE VIEW inactive_drivers AS
SELECT DISTINCT e.id, e.name
FROM drivers d
	LEFT JOIN active_locomotives l ON l.serial = d.locomotive_serial
	JOIN employees e ON d.employee_id = e.id
WHERE l.serial IS NULL
ORDER BY e.id;

-- Active locomotives without drivers
CREATE VIEW locomotives_requiring_driver AS
SELECT DISTINCT l.serial
FROM active_locomotives l
	LEFT JOIN drivers d ON d.locomotive_serial = l.serial
WHERE d.locomotive_serial IS NULL
ORDER BY l.serial;

-- Active locomotives with invalid overlapping trip timetables
CREATE VIEW overscheduled_locomotives AS
SELECT DISTINCT
	a1.serial,
	r1.name AS obstructing_route_name,
	a1.route_id AS obstructing_route_id,
	a1.trip_number AS obstructing_trip_number,
	r2.name AS obstructed_route_name,
	a2.route_id AS obstructed_route_id,
	a2.trip_number AS obstructed_trip_number
FROM locomotive_working_times a1
	JOIN locomotive_working_times a2 USING (serial)
	LEFT JOIN routes r1 ON r1.id = a1.route_id
	LEFT JOIN routes r2 ON r2.id = a2.route_id
WHERE
	-- A locomotive is over-scheduled if the same locomotive...
	a1.serial = a2.serial AND
	-- ...has two different trips...
	NOT (a1.route_id = a2.route_id AND a1.trip_number = a2.trip_number) AND
	-- ...where the start of one trip is between the start and end of another trip.
	a1.start_time BETWEEN a2.start_time AND a2.end_time;


--
-- Create example database roles and users with different permissions and limits
--

CREATE ROLE
	schedule_manager, schedule_viewer,
	vehicle_manager, vehicle_viewer,
	employee_manager, employee_viewer,
	bookings_manager, bookings_viewer;

-- Scheduling data permissions
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON trips TO schedule_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON routes TO schedule_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON visits TO schedule_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON stations TO schedule_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON locomotive_working_times TO schedule_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON locomotive_working_hours TO schedule_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON active_locomotives TO schedule_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON inactive_locomotives TO schedule_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON overscheduled_locomotives TO schedule_manager WITH GRANT OPTION;
GRANT SELECT ON trips TO schedule_viewer;
GRANT SELECT ON routes TO schedule_viewer;
GRANT SELECT ON visits TO schedule_viewer;
GRANT SELECT ON stations TO schedule_viewer;
GRANT SELECT ON locomotive_working_times TO schedule_viewer;
GRANT SELECT ON locomotive_working_hours TO schedule_viewer;
GRANT SELECT ON active_locomotives TO schedule_viewer;
GRANT SELECT ON inactive_locomotives TO schedule_viewer;
GRANT SELECT ON overscheduled_locomotives TO schedule_viewer;

-- Vehicle data permissions
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON locomotives TO vehicle_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON locomotive_working_times TO vehicle_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON locomotive_working_hours TO vehicle_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON active_locomotives TO vehicle_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON inactive_locomotives TO vehicle_manager WITH GRANT OPTION;
GRANT SELECT ON locomotives TO vehicle_viewer;
GRANT SELECT ON locomotive_working_times TO vehicle_viewer;
GRANT SELECT ON locomotive_working_hours TO vehicle_viewer;
GRANT SELECT ON active_locomotives TO vehicle_viewer;
GRANT SELECT ON inactive_locomotives TO vehicle_viewer;

-- Employee data permissions
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON employees TO employee_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON cashiers TO employee_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON drivers TO employee_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON active_locomotives TO employee_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON inactive_locomotives TO employee_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON inactive_drivers TO employee_manager WITH GRANT OPTION;
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON locomotives_requiring_driver TO employee_manager WITH GRANT OPTION;
GRANT SELECT ON employees TO employee_viewer;
GRANT SELECT ON cashiers TO employee_viewer;
GRANT SELECT ON drivers TO employee_viewer;
GRANT SELECT ON active_locomotives TO employee_viewer;
GRANT SELECT ON inactive_locomotives TO employee_viewer;
GRANT SELECT ON inactive_drivers TO employee_viewer;
GRANT SELECT ON locomotives_requiring_driver TO employee_viewer;

-- Bookings data permissions
GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES ON bookings TO bookings_manager WITH GRANT OPTION;
GRANT SELECT ON bookings TO bookings_viewer;

-- Create manager users
CREATE USER
	"timetable_terry"@"localhost" IDENTIFIED BY "superpassword",
	"choochoomad"@"localhost" IDENTIFIED BY "123password",
	"bossman"@"localhost" IDENTIFIED BY "password1",
	"thebookguy"@"localhost" IDENTIFIED BY "qwerty"
WITH
	MAX_QUERIES_PER_HOUR 600000
	MAX_UPDATES_PER_HOUR 60000;

GRANT schedule_manager TO "timetable_terry"@"localhost";
GRANT vehicle_manager TO "choochoomad"@"localhost";
GRANT employee_manager TO "bossman"@"localhost";
GRANT bookings_manager TO "thebookguy"@"localhost";
SET DEFAULT ROLE schedule_manager TO "timetable_terry"@"localhost";
SET DEFAULT ROLE vehicle_manager TO "choochoomad"@"localhost";
SET DEFAULT ROLE employee_manager TO "bossman"@"localhost";
SET DEFAULT ROLE bookings_manager TO "thebookguy"@"localhost";

-- Create viewer users
CREATE USER
	"schedule_mayham"@"localhost" IDENTIFIED BY "hunter2",
	"training_trains"@"localhost" IDENTIFIED BY "incorrect",
	"smallfry"@"localhost" IDENTIFIED BY "_",
	"justapage"@"localhost" IDENTIFIED BY "neverguess"
WITH
	MAX_QUERIES_PER_HOUR 600
	MAX_UPDATES_PER_HOUR 0;

GRANT schedule_viewer TO "schedule_mayham"@"localhost";
GRANT vehicle_viewer TO "training_trains"@"localhost";
GRANT employee_viewer TO "smallfry"@"localhost";
GRANT bookings_viewer TO "justapage"@"localhost";
SET DEFAULT ROLE schedule_viewer TO "schedule_mayham"@"localhost";
SET DEFAULT ROLE vehicle_viewer TO "training_trains"@"localhost";
SET DEFAULT ROLE employee_viewer TO "smallfry"@"localhost";
SET DEFAULT ROLE bookings_viewer TO "justapage"@"localhost";


--
-- Keep an audit of users who change employee data
--

CREATE TABLE employee_data_audit (
	id INT UNSIGNED AUTO_INCREMENT,
	employee_id INT UNSIGNED NOT NULL,
	user VARCHAR(255) NOT NULL,
	date DATETIME NOT NULL,
	action VARCHAR(255) DEFAULT NULL,
	PRIMARY KEY (id)
);

CREATE TRIGGER insert_audit AFTER INSERT ON employees FOR EACH ROW
INSERT INTO employee_data_audit (employee_id, user, date, action) VALUES (NEW.id, USER(), NOW(), "INSERT");

CREATE TRIGGER update_audit AFTER UPDATE ON employees FOR EACH ROW
INSERT INTO employee_data_audit (employee_id, user, date, action) VALUES (OLD.id, USER(), NOW(), "UPDATE");

CREATE TRIGGER delete_audit AFTER DELETE ON employees FOR EACH ROW
INSERT INTO employee_data_audit (employee_id, user, date, action) VALUES (OLD.id, USER(), NOW(), "DELETE");


--
-- Insert test data
--

INSERT INTO
	locomotives (serial, model, build_date, colour)
VALUES
	("32331", "IE 201",      "1994-12-16", "Grey"),
	("32332", "IE 201",      "1994-12-16", "Grey"),
	("32345", "IE 201",      "1995-02-26", "Green"),
	("32346", "IE 201",      "1995-03-05", "Green"),
	("32348", "IE 201",      "1995-03-10", "Green"),
	("32349", "IE 201",      "1995-03-10", "Green"),
	("26285", "CIE 001",     "1956-04-01", "Red"),
	("26286", "CIE 001",     "1956-04-01", "Red"),
	("4231",  "CIE No. CC1", STR_TO_DATE("2/7/1957", "%d/%m/%Y"), "Green");

INSERT INTO
	stations (code, road, city, name)
VALUES
	("HSTON", "St John's Road West", "Dublin",    "Heuston"),
	("CNLLY", "Amiens Street",       "Dublin",    "Connolly"),
	("PERSE", "Westland Row",        "Dublin",    "Pearse"),
	("DGHDA", "Dublin Road",         "Drogheda",  "MacBride"),
	("DDALK", "Carrickmacross Road", "Dundalk",   "Clarke"),
	("BFSTC", "East Bridge Street",  "Belfast",   "Lanyon Place"),
	("BRAY",  "Florence Road",       "Bray",      "Daly"),
	("WXFRD", "Redmond Square",      "Wexford",   "O Hanrahan"),
	("WFORD", "Terminus Street",     "Waterford", "Plunkett"),
	("LMRKJ", "N24",                 NULL,        "Limerick Junction"),
	("LMRCK", "Parnell Street",      "Limerick",  "Colbert"),
	("CORK",  "Lower Glanmire Road", "Cork",      "Kent"),
	("TRLEE", "North Circular Road", "Tralee",    "Casement"),
	("GALWY", "Station Road",        "Galway",    "Ceannt");

INSERT INTO
	employees (name, phone_number, email, wage, working_hours)
VALUES
	("Harry O'Conor",   "0875346789", "hhhhharry@gmail.com",       12.50, 8),
	("Michael MacBride", "0877049328", "michaelmacbride@gmail.com", 12.50, 8),
	("Steven Smith",     NULL,         "thesteve@yahoo.co.uk",      14.90, 8),
	("John McKeown",     "0860138975", NULL,                        16.00, 10),
	("Mary O'Donnell",   "0862077580", "mdonnel88@gmail.com",       13.20, 8),
	("Margot Grellan",   "0879807745", "margo_don@hotmail.com",     12.50, 8),
	("Tom Brady",        "0829073248", "tbrady@yahoo.co.uk",        14.90, 7),
	("Mary Harris",      "0838973240", "mary@harris.ie",            14.90, 8),
	("Daisy Butcher",    "0872347092", "daisy_butcher@gmail.com",   12.50, 8),
	("Niall Morgan",     "0875689012", "niallmorgan87@hotmail.com", 14.90, 8),
	("David Morgan",     "0875689012", "davidmorgan92@hotmail.com", 14.90, 8),
	("Paul Allen",       "0851278932", "psycho@dorsia.com",         14.90, 6),
	("Larry Smokes",     "0861023492", "railwaylarry@gmail.com",    14.90, 8);

INSERT INTO
	cashiers (employee_id, station_code, register)
VALUES
	(1, "HSTON", 2),
	(2, "HSTON", 3),
	(5, "CNLLY", 1),
	(6, "CNLLY", 3),
	(9, "PERSE", 3);

INSERT INTO
	drivers (employee_id, locomotive_serial)
VALUES
	(3,  "32332"),
	(4,  "26285"),
	(7,  "26286"),
	(8,  "32345"),
	(10, "4231"),
	(11, "32331"),
	(12, "32346"),
	(13, NULL);

INSERT INTO
	routes (name)
VALUES
	("Dublin Belfast InterCity"),
	("Dublin Tralee InterCity"),
	("Dublin Waterford InterCity"),
	("East Coast Commuter"),
	("Galway Limerick InterCity"),
	("Dublin Limerick InterCity"),
	("Dublin City");

INSERT INTO
	visits (route_id, stop_number, station_code, platform, fare, arrival_time, departure_time)
VALUES
	-- Dublin Belfast
	(1, 1, "CNLLY", 2, 0.00, NULL,   "0:00"),
	(1, 2, "DGHDA", 1, 4.00, "0:40", "0:45"),
	(1, 3, "DDALK", 1, 2.50, "1:00", "1:05"),
	(1, 4, "BFSTC", 2, 4.50, "2:00", "3:00"),
	(1, 5, "DDALK", 2, 4.50, "3:55", "4:00"),
	(1, 6, "DGHDA", 2, 2.50, "4:30", "4:35"),
	(1, 7, "CNLLY", 2, 4.00, "5:00", NULL),
	-- Dublin Tralee
	(2, 1, "HSTON", 1, 0.00, NULL,   "0:00"),
	(2, 2, "LMRKJ", 1, 5.00, "1:30", "1:40"),
	(2, 3, "TRLEE", 1, 3.50, "2:30", "3:30"),
	(2, 4, "LMRKJ", 1, 3.50, "4:30", "4:50"),
	(2, 5, "HSTON", 1, 5.00, "5:30", NULL),
	-- Dublin Waterford
	(3, 1, "HSTON", 1, 0.00, NULL,   "0:00"),
	(3, 2, "WXFRD", 1, 5.00, "1:20", "1:25"),
	(3, 3, "WFORD", 1, 1.50, "1:50", "2:20"),
	(3, 4, "WXFRD", 1, 1.50, "2:55", "3:00"),
	(3, 5, "HSTON", 1, 5.00, "4:30", NULL),
	-- East Coast
	(4, 1, "DDALK", 1, 0.00, NULL,   "0:00"),
	(4, 2, "DGHDA", 1, 2.00, "0:30", "0:35"),
	(4, 3, "CNLLY", 1, 2.50, "1:00", "1:05"),
	(4, 4, "PERSE", 1, 1.00, "1:10", "1:15"),
	(4, 5, "BRAY",  1, 1.00, "1:30", "1:45"),
	(4, 6, "PERSE", 1, 1.00, "2:00", "2:05"),
	(4, 7, "CNLLY", 1, 1.00, "2:10", "2:15"),
	(4, 8, "DGHDA", 1, 2.50, "2:50", "2:55"),
	(4, 9, "DDALK", 1, 2.00, "3:20", NULL),
	-- Galway Limerick
	(5, 1, "GALWY", 1, 0.00, NULL,   "0:00"),
	(5, 2, "LMRCK", 1, 2.50, "0:50", "1:20"),
	(5, 3, "GALWY", 1, 2.50, "2:10", NULL),
	-- Dublin Limerick
	(6, 1, "HSTON", 1, 0.00, NULL,   "0:00"),
	(6, 2, "LMRKJ", 1, 5.00, "1:30", "1:40"),
	(6, 3, "LMRCK", 1, 2.00, "2:40", "3:00"),
	(6, 4, "LMRKJ", 1, 2.00, "4:20", "4:30"),
	(6, 5, "HSTON", 1, 5.00, "6:00", NULL),
	-- Dublin City
	(7, 1, "CNLLY", 1, 0.00, NULL,   "0:00"),
	(7, 2, "HSTON", 1, 1.00, "0:12", "0:15"),
	(7, 3, "PERSE", 1, 1.00, "0:29", "0:32"),
	(7, 4, "CNLLY", 1, 1.00, "0:40", NULL);

INSERT INTO
	trips (route_id, trip_number, locomotive_serial, start_time)
VALUES
	-- Dublin Belfast
	(1, 1, "32331", "9:00"),
	(1, 2, "32332", "13:00"),
	-- Dublin Tralee
	(2, 1, "32332", "10:00"),
	(2, 2, "32331", "15:00"),
	-- Dublin Waterford
	(3, 1, "32345", "9:30"),
	(3, 2, "32346", "15:00"),
	-- East Coast
	(4, 1, "32348", "7:30"),
	(4, 2, "32349", "9:30"),
	(4, 3, "32348", "12:30"),
	(4, 4, "32349", "14:30"),
	-- Galway Limerick
	(5, 1, "26285", "12:00"),
	-- Dublin Limerick
	(6, 1, "32346", "8:30"),
	(6, 2, "32345", "15:30"),
	-- Dublin City
	(7, 1, "4231", "7:00"),
	(7, 2, "4231", "9:00"),
	(7, 3, "4231", "11:00"),
	(7, 4, "4231", "13:00"),
	(7, 5, "4231", "15:00"),
	(7, 6, "4231", "17:00"),
	(7, 7, "4231", "19:00");

INSERT INTO
	bookings (route_id, trip_number, coach, seat, name, phone_number, email)
VALUES
	(1, 1, "D", 32, "Matt O'Hare",      NULL,         "mhare12@gmail.com"),
	(1, 1, "E", 53, NULL,               NULL,         "askhjldy@sharklazers.com"),
	(2, 1, "D", 23, "Sarah Greenwood",  "0872712348", NULL),
	(2, 2, "B", 2,  "Liam Blanch",      "0861237895", "highlandcastle@live.com"),
	(3, 2, "C", 21, "Joseph Mulligan",  NULL,         "ledlow@gmail.com"),
	(4, 1, "D", 29, "Conor Stevenson", "0861238232", "cstevenson@gmail.com"),
	(4, 4, "D", 12, "Conor Stevenson", "0861238232", "cstevenson@gmail.com"),
	(5, 1, "B", 12, "Trevor Moore",     NULL,         "trevor@moore.com"),
	(7, 4, "B", 21, "Hannah Youth",     "0411237980", "ayouth@tcd.ie");


/* BELOW IS A BUNCH OF EXAMPLE QUERIES I USE TO TEST OUT THE DATABASE */
/* Disable them if you're not interested */

-- Try out all the views
SELECT * FROM locomotive_working_times;
SELECT * FROM locomotive_working_hours;
SELECT * FROM active_locomotives;
SELECT * FROM inactive_locomotives;
SELECT * FROM inactive_drivers;
SELECT * FROM locomotives_requiring_driver;
SELECT * FROM overscheduled_locomotives;

-- When is anybody called Conor commuting using our service?
SET @match := "Conor %";
SELECT b.*, r.name AS route_name, t.start_time AS route_start_time
FROM bookings b
	JOIN trips t USING (route_id, trip_number)
	LEFT JOIN routes r ON r.id = b.route_id
WHERE b.name LIKE @match
ORDER BY t.start_time;

-- Find trips near noon to get from Connolly (CNLLY) to Dundalk (DDALK)
SET
	@source_station := "CNLLY",
	@destination_station := "DDALK",
	@departs_after := "11:00",
	@departs_before := "14:00";
SELECT
	r.name AS route_name,
	r.id AS route_id,
	t.trip_number,
	ADDTIME(t.start_time, v1.departure_time) AS departure_time,
	ADDTIME(t.start_time, v2.arrival_time) AS arrival_time
FROM routes r
	JOIN visits v1 ON v1.route_id = r.id
	JOIN visits v2 ON v2.route_id = r.id
	JOIN trips t ON (t.route_id = r.id)
WHERE
	-- A trip should be valid if its route...
	v1.route_id = v2.route_id AND
	-- ...visits both required stations..
	v1.station_code = @source_station AND v2.station_code = @destination_station AND
	-- ...in the correct order...
	v1.stop_number < v2.stop_number AND
	-- ...with the trip leaving at the right time from the source station.
	ADDTIME(t.start_time, v1.departure_time) BETWEEN @departs_after AND @departs_before
ORDER BY ADDTIME(t.start_time, v2.departure_time);

