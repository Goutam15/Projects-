-- Create fact_table with a composite primary key (PK) and foreign keys (FK)
CREATE TABLE fact_table (
    ims_org_id VARCHAR(50),  -- Foreign key referencing Dimension1 (ims_org_id)
    bed_id INT,  -- Foreign key referencing Dimension2 (bed_id)
    license_beds INT,  -- Number of licensed beds
    census_beds INT,  -- Number of census beds
    staffed_beds INT,  -- Number of staffed beds
    PRIMARY KEY (ims_org_id, bed_id),  -- Composite primary key
    FOREIGN KEY (ims_org_id) REFERENCES Dimension1(ims_org_id),  -- FK referencing Dimension1
    FOREIGN KEY (bed_id) REFERENCES Dimension2(bed_id)  -- FK referencing Dimension2
);

-- Create table for bed_type with primary key (PK)
CREATE TABLE Dimension2 (
    bed_id INT PRIMARY KEY,  -- Primary key
    bed_code VARCHAR(10),  -- Code representing the bed type
    bed_desc VARCHAR(50)  -- Description of the bed type
);

-- Create table for business with primary key (PK)
CREATE TABLE Dimension1 (
    ims_org_id VARCHAR(50) PRIMARY KEY,  -- Primary key
    business_name VARCHAR(255),  -- Name of the hospital or medical center
    ttl_license_beds INT,  -- Total number of licensed beds
    ttl_census_beds INT,  -- Total number of census beds
    ttl_staffed_beds INT,  -- Total number of staffed beds
    bed_cluster_id INT  -- Identifier for the bed cluster
);

-- Verify data insertion by selecting first 3 rows from Dimension1
SELECT * FROM Dimension1 LIMIT 3;

-- Verify data insertion by selecting first 3 rows from Dimension2
select * from Dimesion2 limit 3;

-- Verify data insertion by selecting first 3 rows from fact_table
SELECT * FROM fact_table LIMIT 3;

-- Select the first three facts (measurable quantities) from the fact_table
SELECT
    license_beds,  -- Number of licensed beds
    census_beds,  -- Number of census beds
    staffed_beds  -- Number of staffed beds
FROM
    fact_table
LIMIT 10;  -- Limit the result to the first 10 rows

-- Query to find the top 10 hospitals for SICU and ICU beds based on licensed beds, census beds, and staffed beds
WITH SICUBedDetails AS (
    SELECT
        f.ims_org_id,  -- Identifier for the hospital
        b.business_name,  -- Name of the hospital
        SUM(CASE WHEN d.bed_code = 'IC' THEN f.license_beds ELSE 0 END) AS LicBeds_ICU,  -- Total licensed ICU beds
        SUM(CASE WHEN d.bed_code = 'IC' THEN f.census_beds ELSE 0 END) AS CensBeds_ICU,  -- Total census ICU beds
        SUM(CASE WHEN d.bed_code = 'IC' THEN f.staffed_beds ELSE 0 END) AS StafBeds_ICU,  -- Total staffed ICU beds
        SUM(CASE WHEN d.bed_code = 'SI' THEN f.license_beds ELSE 0 END) AS LicBeds_SICU,  -- Total licensed SICU beds
        SUM(CASE WHEN d.bed_code = 'SI' THEN f.census_beds ELSE 0 END) AS CensBeds_SICU,  -- Total census SICU beds
        SUM(CASE WHEN d.bed_code = 'SI' THEN f.staffed_beds ELSE 0 END) AS StafBeds_SICU  -- Total staffed SICU beds
    FROM
        fact_table f
    JOIN
        Dimesion2 d ON f.bed_id = d.bed_id  -- Join with Dimension2 on bed_id
    JOIN
        Dimension1 b ON f.ims_org_id = b.ims_org_id  -- Join with Dimension1 on ims_org_id
    WHERE
        d.bed_code IN ('SI', 'IC')  -- Filter for SICU and ICU bed codes
    GROUP BY
        f.ims_org_id, b.business_name  -- Group by hospital identifier and name
)
SELECT
    ims_org_id,  -- Identifier for the hospital
    business_name,  -- Name of the hospital
    LicBeds_ICU,  -- Total licensed ICU beds
    LicBeds_SICU,  -- Total licensed SICU beds
    CensBeds_ICU,  -- Total census ICU beds
    CensBeds_SICU,  -- Total census SICU beds
    StafBeds_ICU,  -- Total staffed ICU beds
    StafBeds_SICU  -- Total staffed SICU beds
FROM
    SICUBedDetails
ORDER BY
    LicBeds_SICU DESC,  -- Order by licensed SICU beds in descending order
    CensBeds_SICU DESC,  -- Order by census SICU beds in descending order
    StafBeds_SICU DESC,  -- Order by staffed SICU beds in descending order
    LicBeds_ICU DESC,  -- Order by licensed ICU beds in descending order
    CensBeds_ICU DESC,  -- Order by census ICU beds in descending order
    StafBeds_ICU DESC  -- Order by staffed ICU beds in descending order
LIMIT 10;  -- Limit the result to the top 10 hospitals
