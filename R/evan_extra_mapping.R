#Mapping part of app

library(shiny)
library(tidyverse)
library(tmap)
library(spData)
library(spDataLarge)
library(sf)
library(broom)

# Load Data
parties <- read_rds("../data/political_parties_of_states.rds")
ctracking <- read_rds("../data/ctracking.rds")

# combine data sets
ctracking <- left_join(ctracking, parties, by = "state")

#making the New Progressive/Republican Republican
ctracking$Party[ctracking$Party == "New Progressive/Republican"] <- "Republican"

# rearange dataset for readability
ctracking <- ctracking %>%
    select(state, Name, Month, Party, everything())

#BEGIN CONSTRUCTING SHINY APP
ui <- fluidPage(

    sidebarLayout(
        sidebarPanel(
            varSelectInput("mapvar", "Please select a Variable", data = ctracking,
                           selected = "death")
        ), #end sidebarPanel
        mainPanel(
            tmapOutput("map", height = 500),
            verbatimTextOutput("map_anova")
        ) #end mainPanel
    )  # End sidebarLaoyut
)

server <- function(input, output, session) {
  output$map <- renderTmap({
      tmap_mode("view")
      tm_shape(us_states, projection = 2163) +
          tm_polygons() +
          tm_layout(frame = FALSE)
  }) # End renderTmap
  
  output$map_anova <- renderPrint({
      summary(aov(ctracking[[input$mapvar]] ~ Party, data = ctracking))
  }) # End renderTable
}

shinyApp(ui, server)