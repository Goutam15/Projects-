CREATE TABLE Dim_Date (
    Date_Key INT PRIMARY KEY,
    Date DATE,
    Year INT,
    Month INT,
    Quarter INT
);

-- Create Location Dimension Table
CREATE TABLE Dim_Location (
    Location_Key INT AUTO_INCREMENT PRIMARY KEY,
    Zip_Code VARCHAR(10),
    City VARCHAR(50),
    County VARCHAR(50)
);

-- Create Age Group Dimension Table
CREATE TABLE Dim_Age_Group (
    Age_Group_Key INT AUTO_INCREMENT PRIMARY KEY,
    Age_Group VARCHAR(10)
);

-- Create Gender Dimension Table
CREATE TABLE Dim_Gender (
    Gender_Key INT AUTO_INCREMENT PRIMARY KEY,
    Gender VARCHAR(10)
);

-- Create Race/Ethnicity Dimension Table
CREATE TABLE Dim_Race_Ethnicity (
    Race_Ethnicity_Key INT AUTO_INCREMENT PRIMARY KEY,
    Race_Ethnicity VARCHAR(50)
);

-- Create Income Level Dimension Table
CREATE TABLE Dim_Income (
    Income_Level_Key INT AUTO_INCREMENT PRIMARY KEY,
    Income_Level VARCHAR(50)
);

-- Create Education Level Dimension Table
CREATE TABLE Dim_Education (
    Education_Level_Key INT AUTO_INCREMENT PRIMARY KEY,
    Education_Level VARCHAR(50)
);

-- Create Fact Table for Alcohol Consumption
CREATE TABLE Fact_Alcohol_Consumption (
    Alcohol_Consumption_ID INT AUTO_INCREMENT PRIMARY KEY,
    Date_Key INT,
    Location_Key INT,
    Age_Group_Key INT,
    Gender_Key INT,
    Race_Ethnicity_Key INT,
    Income_Level_Key INT,
    Education_Level_Key INT,
    Heavy_Drinker CHAR(3),
    Sample_Size INT,
    Data_Value INT,
    Prevalence FLOAT,
    FOREIGN KEY (Date_Key) REFERENCES Dim_Date(Date_Key),
    FOREIGN KEY (Location_Key) REFERENCES Dim_Location(Location_Key),
    FOREIGN KEY (Gender_Key) REFERENCES Dim_Gender(Gender_Key),
    FOREIGN KEY (Race_Ethnicity_Key) REFERENCES Dim_Race_Ethnicity(Race_Ethnicity_Key),
    FOREIGN KEY (Income_Level_Key) REFERENCES Dim_Income(Income_Level_Key),
    FOREIGN KEY (Education_Level_Key) REFERENCES Dim_Education(Education_Level_Key)
);
select * from `brfss_ok new`;

SELECT * FROM Dim_Location



