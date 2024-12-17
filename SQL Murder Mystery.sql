---Crime Scene Report Query
SELECT * FROM crime_scene_report WHERE date = '20180115' AND type = 'murder' AND city = 'SQL City';
----1st Witness who lives at the last house which is retrieved by the address_number
SELECT * FROM person WHERE address_street_name ='Northwestern Dr' ORDER BY address_number DESC ;
----2nd Witness 
SELECT * FROM person WHERE  name like '%Annabel%' and address_street_name ='Franklin Ave' ORDER BY address_number DESC ;

SELECT * FROM interview WHERE person_id = "14887";

SELECT * FROM interview WHERE person_id = "16371";

SELECT * FROM get_fit_now_member WHERE id LIKE '48Z%';
SELECT * FROM drivers_license WHERE plate_number LIKE '%H42W%';

SELECT p.* FROM person p JOIN get_fit_now_member m ON p.id = m.person_id JOIN drivers_license dl ON p.license_id = dl.id WHERE m.id LIKE '48Z%' AND dl.plate_number LIKE '%H42W%';

---Jeremy Bowers' Gym Activities Query

SELECT gfm.id, gfm.name, gfc.check_in_date, gfc.check_in_time, gfc.check_out_time FROM get_fit_now_member gfm JOIN get_fit_now_check_in gfc ON gfm.id = gfc.membership_id WHERE gfm.person_id = "Jeremy Bowers.person_id";

SELECT gfc.membership_id, gfc.check_in_date, fec.event_id, fec.event_name, fec.date FROM get_fit_now_check_in gfc JOIN facebook_event_checkin fec ON gfc.check_in_date = fec.date WHERE gfc.membership_id = '48Z55';

---Income Data Investigation Query
SELECT annual_income FROM income WHERE ssn = "Jeremy Bowers SSN";

---Miranda Priestly's Car and Physical Description Query
SELECT dl.id, dl.car_make, dl.car_model FROM drivers_license dl JOIN person p ON dl.id = p.license_id WHERE p.name = 'Miranda Priestly';

---Physical Description Query: 
SELECT dl.id, dl.height, dl.hair_color, dl.eye_color FROM drivers_license dl JOIN person p ON dl.id = p.license_id WHERE p.name = 'Miranda Priestly';

---Query to Confirm Miranda Priestly's Car Make and Model
SELECT dl.id, dl.car_make, dl.car_model
FROM drivers_license dl
JOIN person p ON dl.id = p.license_id
WHERE p.name = 'Miranda Priestly';

---Query to Show Miranda Priestly's Body Descriptions
SELECT dl.id, dl.height, dl.hair_color, dl.eye_color
FROM drivers_license dl
JOIN person p ON dl.id = p.license_id
WHERE p.name = 'Miranda Priestly';

---Check Event Attendance
SELECT p.name, fec.event_id, COUNT(*)
FROM facebook_event_checkin fec
JOIN person p ON fec.person_id = p.id
JOIN drivers_license dl ON p.license_id = dl.id
WHERE dl.car_make = 'Tesla' AND dl.car_model = 'Model S' AND dl.hair_color = 'red'
GROUP BY p.name, fec.event_id
HAVING COUNT(*) > 1;

---Examine Miranda Priestly's Connections and Activities
SELECT p.name, gfnm.id, gfnm.membership_start_date, gfnm.membership_status, gfn.check_in_date, gfn.check_in_time
FROM person p
JOIN get_fit_now_member gfnm ON p.id = gfnm.person_id
JOIN get_fit_now_check_in gfn ON gfnm.id = gfn.membership_id
WHERE p.name = 'Miranda Priestly'
ORDER BY gfn.check_in_date, gfn.check_in_time;

---Crime Scene Report Query:

SELECT * FROM crime_scene_report WHERE date = '20180115' AND type = 'murder' AND city = 'SQL City';
SELECT * FROM interview WHERE person_id = "Witness ID";
SELECT * FROM get_fit_now_member WHERE id LIKE '48Z%';
SELECT * FROM drivers_license WHERE plate_number LIKE '%H42W%';
SELECT p.* FROM person p 
JOIN get_fit_now_member m ON p.id = m.person_id 
JOIN drivers_license dl ON p.license_id = dl.id 
WHERE m.id LIKE '48Z%' AND dl.plate_number LIKE '%H42W%';

---Jeremy Bowers' Gym Activities Query:
SELECT gfm.id, gfm.name, gfc.check_in_date, gfc.check_in_time, gfc.check_out_time 
FROM get_fit_now_member gfm 
JOIN get_fit_now_check_in gfc ON gfm.id = gfc.membership_id 
WHERE gfm.person_id = "Jeremy Bowers' Person ID";

---Cross-Referencing Gym and Event Check-ins Query:

SELECT gfc.membership_id, gfc.check_in_date, fec.event_id, fec.event_name, fec.date 
FROM get_fit_now_check_in gfc 
JOIN facebook_event_checkin fec ON gfc.check_in_date = fec.date 
WHERE gfc.membership_id = '48Z55';

---Income Data Investigation Query:

SELECT annual_income FROM income WHERE ssn = "Jeremy Bowers' SSN";

---Miranda Priestly's Car and Physical Description Query:
SELECT dl.id, dl.car_make, dl.car_model FROM drivers_license dl 
JOIN person p ON dl.id = p.license_id 
WHERE p.name = 'Miranda Priestly';

---Physical Description Query:
SELECT dl.id, dl.height, dl.hair_color, dl.eye_color FROM drivers_license dl 
JOIN person p ON dl.id = p.license_id 
WHERE p.name = 'Miranda Priestly';

---Check Event Attendance Query:

SELECT p.name, fec.event_id, COUNT(*)
FROM facebook_event_checkin fec
JOIN person p ON fec.person_id = p.id
JOIN drivers_license dl ON p.license_id = dl.id
WHERE 	dl.car_make = 'Tesla' AND dl.car_model = 'Model S' AND dl.hair_color = 'red'
GROUP BY p.name, fec.event_id
HAVING COUNT(*) > 1;

SELECT p.name, gfnm.id, gfnm.membership_start_date, gfnm.membership_status, gfn.check_in_date, gfn.check_in_time
FROM person p
JOIN get_fit_now_member gfnm ON p.id = gfnm.person_id
JOIN get_fit_now_check_in gfn ON gfnm.id = gfn.membership_id
WHERE p.name = 'Miranda Priestly'
ORDER BY gfn.check_in_date, gfn.check_in_time;