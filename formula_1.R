#install.packages(c("tidyverse", "ggplot2"))

library(tidyverse)
library(ggplot2)

setwd("~/Documents/Egyetem/_MSc/3_szemeszter/erasmus/information_visualization/project/")

results <- read.csv("f1_dataset/results.csv", sep = ",")
drivers <- read.csv("f1_dataset/drivers.csv", sep = ",")
qualifying <- read.csv("f1_dataset/qualifying.csv", sep = ",")
races <- read.csv("f1_dataset/races.csv", sep = ",")
circuits <- read.csv("f1_dataset/circuits.csv", sep = ",")
constructors <- read.csv("f1_dataset/constructors.csv", sep = ",")
constructorStandings <- read.csv("f1_dataset/constructor_standings.csv", sep = ",")
driverStandings <- read.csv("f1_dataset/driver_standings.csv", sep = ",")
pitStops <- read.csv("f1_dataset/pit_stops.csv", sep = ",")

numberOfWinsByDrivers <- function() {
  
  results.filtered <- results %>%
    select(raceId, driverId, positionOrder) %>%
    filter(, positionOrder == "1")
  
  drivers.filtered <- drivers %>%
    select(driverId, driverRef)
  
  summarise <- merge(x = results.filtered, y = drivers.filtered, by = "driverId") %>%
    group_by(driverId) %>%
    summarise(sum = sum(positionOrder)) %>%
    filter(, sum >= 25)
  
  exampleTask <- merge(x = drivers.filtered, y = summarise, by = "driverId") %>%
    arrange(desc(sum)) %>%
    select(driverRef, sum)
  
  ggplot(exampleTask, aes(x = reorder(driverRef, -sum), y = sum, fill = driverRef)) +
    geom_bar(stat = "identity", width = 0.7) +
    labs(x = "Driver", y = "Number of Wins", title = "Number of Wins by Drivers") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
  
}

poleSitterBestTime <- function() {
  
  qualifying.filtered <- qualifying %>%
    select(raceId, position, q1, q2, q3) %>%
    filter(, position == "1")
  
  qualifying.filtered$q1 <- replace(qualifying.filtered$q1, qualifying.filtered$q1 < 0, NA)
  qualifying.filtered$q2 <- replace(qualifying.filtered$q2, qualifying.filtered$q2 < 0, NA)
  qualifying.filtered$q3 <- replace(qualifying.filtered$q3, qualifying.filtered$q3 < 0, NA)
  
  races.filtered <- races %>%
    select(raceId, circuitId, year)
  
  circuits.filtered <- circuits %>%
    select(circuitId, circuitRef)
  
  qualifying.merge1 <- merge(x = races.filtered, y = circuits.filtered, by = "circuitId")
  
  qualifying.merge2 <- merge(x = qualifying.merge1, y = qualifying.filtered, by = "raceId") %>%
    mutate(bestTime = pmin(q1, q2, q3, na.rm=TRUE)) %>%
    select(circuitRef, year, bestTime) %>%
    filter(circuitRef == "monaco") %>%
    arrange(., year)
  
  #View(qualifying.merge2)
  
  ggplot(qualifying.merge2, aes(x = year, y = bestTime, group = 1)) +
    geom_line() +
    geom_point() +
    geom_smooth() +
    labs(x = "Year", y = "Best Time of Pole Sitter",
         title = "Best Time of Pole Sitter by Year") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
  
}

constructorChampionships <- function() {
  
  racesFinal <- races %>% group_by(year) %>%
    filter(round == max(round)) %>%
    select(year, raceId) %>%
    arrange(year)
  
  constructorsJoin <- merge(x = constructorStandings, y = racesFinal,
                             by = "raceId", type = "full") %>%
    filter(position == 1) %>%
    select(year, raceId, constructorId, points) %>%
    arrange(year)
  
  constructors.filtered <- constructors %>%
    select(constructorId, constructorRef)
  
  constructorsFinal <- merge(x = constructorsJoin, y = constructors.filtered,
               by = "constructorId", type = "full") %>%
    arrange(year) %>%
    select(year, constructorRef)
  
  constructorsSummarise <- constructorsFinal %>%
    group_by(constructorRef) %>%
    summarise(sum = n())
  
  ggplot(constructorsSummarise, aes(x = reorder(constructorRef, -sum), y = sum,
                                    fill = constructorRef)) +
    geom_bar(stat = "identity", width = 0.7) +
    labs(x = "Constructor", y = "Number of Championships",
         title = "Number of Constructor Championships") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
  
}

driverChampionships <- function() {
  
  racesFinal <- races %>% group_by(year) %>%
    filter(round == max(round)) %>%
    select(year, raceId) %>%
    arrange(year)
  
  driversJoin <- merge(x = driverStandings, y = racesFinal,
                            by = "raceId", type = "full") %>%
    filter(position == 1) %>%
    select(year, raceId, driverId, points) %>%
    arrange(year)
  
  drivers.filtered <- drivers %>%
    select(driverId, driverRef)
  
  driversFinal <- merge(x = driversJoin, y = drivers.filtered,
                             by = "driverId", type = "full") %>%
    arrange(year) %>%
    select(year, driverRef)
  
  driversSummarise <- driversFinal %>%
    group_by(driverRef) %>%
    summarise(sum = n()) %>%
    filter(, sum >= 2)
  
  ggplot(driversSummarise, aes(x = reorder(driverRef, -sum), y = sum,
                                    fill = driverRef)) +
    geom_bar(stat = "identity", width = 0.7) +
    labs(x = "Driver", y = "Number of Championships",
         title = "Number of Driver Championships") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
  
}

gridToWinConversion <- function() {
  
  results.filtered <- results %>%
    select(raceId, grid, position) %>%
    filter(position == 1)
  
  races.filtered <- races %>%
    select(raceId, year, circuitId)
  
  gridResultsMerge <- merge(x = results.filtered, y = races.filtered,
                            by = "raceId", type = "full")
  
  circuits.filtered <- circuits %>%
    select(circuitId, circuitRef)
  
  gridResultsCircuitsMerge <- merge(x = gridResultsMerge, y = circuits.filtered,
                                    by = "circuitId", type = "full") %>%
    select(circuitRef, year, grid, position)
  
  gridResultsPlot <- gridResultsCircuitsMerge %>%
    group_by(circuitRef, grid) %>%
    summarise(sum = n()) %>%
    filter(circuitRef == "monaco")
  
  gridResultsPlot$grid <- as.character(gridResultsPlot$grid)
  
  ggplot(gridResultsPlot, aes(x = "", y = sum, fill = reorder(grid, as.integer(grid)))) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_viridis_d()
  
}

numberOfWinsByDrivers()
poleSitterBestTime()
constructorChampionships()
driverChampionships()
gridToWinConversion()
