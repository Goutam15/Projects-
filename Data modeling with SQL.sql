select * from movies;
CREATE TABLE movies (
  Year int DEFAULT NULL,
  Length decimal(5,1) DEFAULT NULL,
  Title varchar(255) DEFAULT NULL,
  Subject varchar(100) DEFAULT NULL,
  Actor varchar(255) DEFAULT NULL,
  Actress varchar(255) DEFAULT NULL,
  Director varchar(255) DEFAULT NULL,
  Popularity decimal(5,1) DEFAULT NULL,
  Awards varchar(10) DEFAULT NULL;
  
  -- Question 1
SELECT Title, Popularity
FROM Movies
ORDER BY Popularity DESC
LIMIT 5;

-- Question 2
SELECT Director, AVG(Popularity) AS AvgPopularity, COUNT(*) AS MovieCount
FROM Movies
GROUP BY Director
HAVING AvgPopularity > 50
ORDER BY AvgPopularity DESC;

-- Question 3
SELECT Subject, AVG(Length) AS AvgLength
FROM Movies
GROUP BY Subject;
