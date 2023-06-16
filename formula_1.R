# Load packages ----------------------------------------------------------------

#install.packages(c("tidyverse", "ggplot2", "shiny"))

library(tidyverse)
library(ggplot2)
library(shiny)

# Load data --------------------------------------------------------------------

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

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Interesting Formula 1 Statistics"),
  
  br(),
  
  sidebarLayout(
  
    sidebarPanel(
      
      sliderInput(inputId = "year",
                  "Select Time Period:",
                  min = 1950,
                  max = 2022,
                  value = c(1950, 2022),
                  ticks = FALSE,
                  sep = ""),
      
      conditionalPanel(condition = "output.showTrack == true",
                     br(),
                     selectInput(
                       inputId = "track",
                       label = "Select Track:",
                       choices = circuits$circuitRef,
                       selected = "monaco"
                     )),
    ),
    
    mainPanel(
      
      tabsetPanel(id = "plotTabs",
                  tabPanel("Driver Wins", value = 1, plotOutput("plot1")),
                  tabPanel("Driver Championships", value = 2, plotOutput("plot2")),
                  tabPanel("Constructor Championships", value = 3, plotOutput("plot3")),
                  tabPanel("Best Lap Times", value = 4, plotOutput("plot4")),
                  tabPanel("Wins From Grid Position", value = 5, plotOutput("plot5"))
      )
    )
  )
)


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$showTrack <- reactive({
    ifelse(input$plotTabs == 4 | input$plotTabs == 5, TRUE, FALSE)
  })
  outputOptions(output, "showTrack", suspendWhenHidden = FALSE)
  
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
      theme(plot.title = element_text(size = 20, face = "bold"),
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
      theme(plot.title = element_text(size = 20, face = "bold"),
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
      theme(plot.title = element_text(size = 20, face = "bold"),
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
      filter(circuitRef == input$track) %>%
      arrange(., year)
    
    ggplot(qualifying.merge2, aes(x = year, y = bestTime, group = 1)) +
      geom_line() +
      geom_point() +
      geom_smooth(method = "loess", formula = "y ~ x") +
      labs(x = "Year", y = "Best Time of Pole Sitter",
           title = "Best Lap Time of Pole Sitter by Year") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, face = "bold"),
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
      summarise(sum = n(), .groups = "keep") %>%
      filter(circuitRef == input$track)
    
    gridResultsPlot$grid <- as.character(gridResultsPlot$grid)
    
    ggplot(gridResultsPlot, aes(x = "", y = sum, fill = reorder(grid, as.integer(grid)))) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      theme_void() +
      theme(plot.title = element_text(size = 20, face = "bold")) +
      labs(fill = "Grid Positions", title = "Wins From Grid Position") +
      scale_fill_viridis_d()
    
  }
  
  output$plot1 <- renderPlot({
    numberOfWinsByDrivers()
  })
  
  output$plot2 <- renderPlot({
    driverChampionships()
  })
  
  output$plot3 <- renderPlot({
    constructorChampionships()
  })
  
  output$plot4 <- renderPlot({
    poleSitterBestTime()
  })
  
  output$plot5 <- renderPlot({
    gridToWinConversion()
  })
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
