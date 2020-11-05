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
ctracking2 <- left_join(ctracking, parties, by = "state")

#making the New Progressive/Republican Republican
ctracking2$Party[ctracking2$Party == "New Progressive/Republican"] <- "Republican"

# rearange dataset for readability
ctracking2 <- ctracking2 %>%
    select(state, Name, Month, Party, death, deathConfirmed, hospitalized,
           onVentilatorCumulative, negative, positive, pending, recovered) %>%
  rename(Confirmed_Deaths = deathConfirmed,
         On_Ventilator = onVentilatorCumulative,
         Negative_Test = negative,
         Positive_Test = positive,
         Pending_Test = pending,
         Recovered = recovered,
         NAME = Name)

#BEGIN CONSTRUCTING SHINY APP
ui <- fluidPage(

    sidebarLayout(
        sidebarPanel(
            varSelectInput("mapvar", "Please select a Variable", data = ctracking2,
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
    validate(need(
      is.numeric(ctracking2[[input$mapvar]]) == "TRUE" & input$mapvar != "Month",
      "Please select a numeric variable"
    ))
      tmap_mode("view")
      tm_shape(us_states) +
          tm_polygons(col = reactive({ctracking2[[input$mapvar]]}))
  }) # End renderTmap
  
  output$map_anova <- renderPrint({
    validate(need(
      is.numeric(ctracking2[[input$mapvar]]) == "TRUE" & input$mapvar != "Month",
      "Please select a numeric variable"
    ))
      summary(aov(ctracking2[[input$mapvar]] ~ Party, data = ctracking2))
  }) # End renderTable
}

shinyApp(ui, server)