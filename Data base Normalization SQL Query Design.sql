-- Drop tables if they exist to ensure a clean slate before creating new ones
DROP TABLE IF EXISTS Movies_Actors;
DROP TABLE IF EXISTS Movies_Actresses;
DROP TABLE IF EXISTS Movies;
DROP TABLE IF EXISTS Subjects;
DROP TABLE IF EXISTS Directors;
DROP TABLE IF EXISTS Actors;
DROP TABLE IF EXISTS Actresses;
DROP TABLE IF EXISTS Genres;

-- Create Subjects Table
CREATE TABLE Subjects (
    SubjectID INT AUTO_INCREMENT PRIMARY KEY,  -- Primary key with auto-increment
    Subject VARCHAR(255) NOT NULL              -- Subject name, required field
);

-- Create Directors Table
CREATE TABLE Directors (
    DirectorID INT AUTO_INCREMENT PRIMARY KEY, -- Primary key with auto-increment
    Director VARCHAR(255) NOT NULL             -- Director name, required field
);

-- Create Actors Table
CREATE TABLE Actors (
    ActorID INT AUTO_INCREMENT PRIMARY KEY,    -- Primary key with auto-increment
    Actor VARCHAR(255) NOT NULL                -- Actor name, required field
);

-- Create Actresses Table
CREATE TABLE Actresses (
    ActressID INT AUTO_INCREMENT PRIMARY KEY,  -- Primary key with auto-increment
    Actress VARCHAR(255) NOT NULL              -- Actress name, required field
);

-- Create Genres Table
CREATE TABLE Genres (
    GenreID INT AUTO_INCREMENT PRIMARY KEY,    -- Primary key with auto-increment
    GenreName VARCHAR(50) NOT NULL             -- Genre name, required field
);

-- Create Movies Table
CREATE TABLE Movies (
    MovieID INT AUTO_INCREMENT PRIMARY KEY,    -- Primary key with auto-increment
    Title VARCHAR(255) NOT NULL,               -- Movie title, required field
    Year INT,                                  -- Release year of the movie
    Length DECIMAL(5, 2),                      -- Length of the movie in minutes
    Popularity DECIMAL(5, 2),                  -- Popularity score of the movie
    Awards VARCHAR(255),                       -- Awards received by the movie
    SubjectID INT,                             -- Foreign key to Subjects table
    DirectorID INT,                            -- Foreign key to Directors table
    GenreID INT,                               -- Foreign key to Genres table
    FOREIGN KEY (SubjectID) REFERENCES Subjects(SubjectID),  -- Define foreign key relationship with Subjects table
    FOREIGN KEY (DirectorID) REFERENCES Directors(DirectorID), -- Define foreign key relationship with Directors table
    FOREIGN KEY (GenreID) REFERENCES Genres(GenreID)           -- Define foreign key relationship with Genres table
);

-- Create Movies_Actors Table to handle many-to-many relationship between movies and actors
CREATE TABLE Movies_Actors (
    MovieID INT,                               -- Foreign key to Movies table
    ActorID INT,                               -- Foreign key to Actors table
    PRIMARY KEY (MovieID, ActorID),            -- Composite primary key
    FOREIGN KEY (MovieID) REFERENCES Movies(MovieID),  -- Define foreign key relationship with Movies table
    FOREIGN KEY (ActorID) REFERENCES Actors(ActorID)   -- Define foreign key relationship with Actors table
);

-- Create Movies_Actresses Table to handle many-to-many relationship between movies and actresses
CREATE TABLE Movies_Actresses (
    MovieID INT,                               -- Foreign key to Movies table
    ActressID INT,                             -- Foreign key to Actresses table
    PRIMARY KEY (MovieID, ActressID),          -- Composite primary key
    FOREIGN KEY (MovieID) REFERENCES Movies(MovieID),  -- Define foreign key relationship with Movies table
    FOREIGN KEY (ActressID) REFERENCES Actresses(ActressID)  -- Define foreign key relationship with Actresses table
);

-- Query 1: Top 5 most popular movies
SELECT Title, Popularity
FROM Movies
ORDER BY Popularity DESC  -- Order by popularity in descending order
LIMIT 5;  -- Limit the result to top 5 records

-- Query 2: Directors with highest average popularity and their movie counts
SELECT D.Director, COUNT(M.MovieID) AS NumberOfMovies, AVG(M.Popularity) AS AveragePopularity
FROM Movies M
JOIN Directors D ON M.DirectorID = D.DirectorID  -- Join with Directors table
GROUP BY D.DirectorID, D.Director  -- Group by director ID and director name
HAVING COUNT(M.MovieID) > 0  -- Ensure director has made at least one movie
ORDER BY AveragePopularity DESC  -- Order by average popularity in descending order
LIMIT 5;  -- Limit the result to top 5 records

-- Query 3: Average movie length for each genre
SELECT S.Subject, AVG(M.Length) AS AverageLength
FROM Movies M
JOIN Subjects S ON M.SubjectID = S.SubjectID  -- Join with Subjects table
GROUP BY S.Subject;  -- Group by subject name

-- Add GenreID column to Movies table
ALTER TABLE Movies
ADD COLUMN GenreID INT;

-- Create the foreign key relationship
ALTER TABLE Movies
ADD CONSTRAINT fk_Movies_Genres
FOREIGN KEY (GenreID)
REFERENCES Genres(GenreID);

-- Query 1: Retrieve the top 5 most popular movies along with their genres and directors
SELECT 
    M.Title,
    M.Popularity,
    G.GenreName,
    D.Director
FROM 
    Movies M
JOIN 
    Genres G ON M.GenreID = G.GenreID  -- Join with Genres table to get genre name
JOIN 
    Directors D ON M.DirectorID = D.DirectorID  -- Join with Directors table to get director name
ORDER BY 
    M.Popularity DESC  -- Order by popularity in descending order
LIMIT 5;  -- Limit the result to top 5 records

-- Query 2: List all movies along with their main actors and genres
SELECT 
    M.Title,
    A.Actor,
    G.GenreName
FROM 
    Movies M
JOIN 
    Movies_Actors MA ON M.MovieID = MA.MovieID  -- Join with Movies_Actors table to get actor IDs
JOIN 
    Actors A ON MA.ActorID = A.ActorID  -- Join with Actors table to get actor names
JOIN 
    Genres G ON M.GenreID = G.GenreID  -- Join with Genres table to get genre name
ORDER BY 
    M.Title;  -- Order by movie title

-- Query 3: Find the average movie length for each director and their respective genres
SELECT 
    D.Director,
    G.GenreName,
    AVG(M.Length) AS AverageLength
FROM 
    Movies M
JOIN 
    Directors D ON M.DirectorID = D.DirectorID  -- Join with Directors table to get director names
JOIN 
    Genres G ON M.GenreID = G.GenreID  -- Join with Genres table to get genre names
GROUP BY 
    D.Director, G.GenreName  -- Group by director and genre
ORDER BY 
    D.Director, G.GenreName;  -- Order by director and genre
