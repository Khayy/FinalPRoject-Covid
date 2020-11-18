#Mapping part of app

library(shiny)
library(tidyverse)
library(tmap)
library(spData)
library(spDataLarge)
library(sf)
library(broom)
library(sf)
library(leaflet)
library(viridis)
library(RColorBrewer)
library(tigris)


# Load Data
parties <- read_rds("../data/political_parties_of_states.rds")
ctracking <- read_rds("../data/ctracking.rds")

# combine data sets
ctracking2 <- left_join(ctracking, parties, by = "state")

#making the New Progressive/Republican Republican
ctracking2$Party[ctracking2$Party == "New Progressive/Republican"] <- "Republican"

# write a function to get what I need/correct it
selection <- function(a) {
  
  q <- data.frame(matrix(nrow = 453, ncol = 44))
  names(q) <- names(a)
  
  for (i in seq_along(1:50)) {
    b <- a %>%
      filter(state == state.abb[i])
    
    # Fix hospitalized
    b$hospitalizedCumulative <- sum(
      ctracking2$hospitalizedCurrently[ctracking2$state == state.abb[i]],
                                    na.rm = T)
    
    b$onVentilatorCumulative <- sum(
      ctracking2$onVentilatorCurrently[ctracking2$state == state.abb[i]],
      na.rm = T)
    
    q <- rbind(q,b)
  }
  
  q <- q[-c(1:453),]
  
  a <- q

} # end function

# apply function to data
ctracking2 <- selection(ctracking2)
  
# rearrange data for readability
ctracking2 <- ctracking2 %>%
    select(Month, state,Name, Party, deathIncrease, hospitalizedCumulative,
           onVentilatorCumulative, negativeIncrease, positiveIncrease, recovered) %>%
  rename(
         On_Ventilator = onVentilatorCumulative,
         Negative_Test = negativeIncrease,
         Positive_Test = positiveIncrease,
         Recovered = recovered,
         NAME = Name,
         Hospitalized = hospitalizedCumulative,
         death = deathIncrease)

# Get info by state
ctracking2 <- ctracking2 %>%
  group_by(state, NAME, Party, recovered) %>%
  summarise(On_Ventilator = sum(On_Ventilator), 
            Negative_Test = sum(Negative_Test),
            Positive_Test = sum(Positive_Test),
            Hospitalized = sum(Hospitalized),
            death = sum(death)) %>%
  select(state, NAME, Party, death, Positive_Test, Negative_Test, everything())

#get the state spacial data
states <- states()

# remove observations in state that are not in ctracking2
states <- semi_join(states, ctracking2, by = c("STUSPS" = "state"))

# combine spatial data to normal data
mapping <- geo_join(states, ctracking2, "NAME", "NAME")


#BEGIN CONSTRUCTING SHINY APP
ui <- fluidPage(

    sidebarLayout(
        sidebarPanel(
            varSelectInput("mapvar", "Please Select a Variable to Map", data = ctracking2,
                           selected = "death"),
            textOutput("Intro"),
            tableOutput("map_anova")
        ), #end sidebarPanel
        mainPanel(
          leafletOutput("map", height = 500),
          textOutput("warning")
            ) #end mainPanel
    )  # End sidebarLaoyut
)


server <- function(input, output, session) {
    
  output$map <- renderLeaflet({
    validate(need(
      is.numeric(ctracking2[[input$mapvar]]) == "TRUE" & input$mapvar != "Month",
      "Please select a numeric variable"
    ))
    
    leaf <- leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(-98.483330, 38.712046, zoom = 4)
    
    if (input$mapvar == "death") {
      # Color palette
      pal <- colorNumeric("Reds", domain=mapping$death)
      
      # Setting up the pop up text
      popup_sb <- paste0(mapping$NAME,": ", mapping$death)
      
      leaf %>%
        addPolygons(data = mapping, 
                    fillColor = ~pal(mapping$death), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = mapping$death, 
                  position = "bottomright")
      
    } else if (input$mapvar == "Hospitalized") {
      
      # Color palette
      pal <- colorNumeric("Reds", domain=mapping$Hospitalized)
      
      # Setting up the pop up text
      popup_sb <- paste0(mapping$NAME,": ", mapping$Hospitalized)
      
      leaf %>%
        addPolygons(data = mapping , 
                    fillColor = ~pal(mapping$Hospitalized), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = mapping$Hospitalized, 
                  position = "bottomright")
      
    } else if (input$mapvar == "On_Ventilator") {
      
      # Color palette
      pal <- colorNumeric("Reds", domain=mapping$On_Ventilator)
      
      # Setting up the pop up text
      popup_sb <- paste0(mapping$NAME,": ", mapping$On_Ventilator)
      
      leaf %>%
        addPolygons(data = mapping , 
                    fillColor = ~pal(mapping$On_Ventilator), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = mapping$On_Ventilator, 
                  position = "bottomright")
      
    } else if (input$mapvar == "Negative_Test") {
      
      # Color palette
      pal <- colorNumeric("Reds", domain=mapping$Negative_Test)
      
      # Setting up the pop up text
      popup_sb <- paste0(mapping$NAME,": ", mapping$Negative_Test)
      
      leaf %>%
        addPolygons(data = mapping , 
                    fillColor = ~pal(mapping$Negative_Test), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = mapping$Negative_Test, 
                  position = "bottomright")
      
    } else if (input$mapvar == "Positive_Test") {
      
      # Color palette
      pal <- colorNumeric("Reds", domain=mapping$Positive_Test)
      
      # Setting up the pop up text
      popup_sb <- paste0(mapping$NAME,": ", mapping$Positive_Test)
      
      leaf %>%
        addPolygons(data = mapping , 
                    fillColor = ~pal(mapping$Positive_Test), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = mapping$Positive_Test, 
                  position = "bottomright")
      
    } else if (input$mapvar == "Recovered") {
      
      # Color palette
      pal <- colorNumeric("Reds", domain=mapping$Recovered)
      
      # Setting up the pop up text
      popup_sb <- paste0(mapping$NAME,": ", mapping$Recovered)
      
      leaf %>%
        addPolygons(data = mapping , 
                    fillColor = ~pal(mapping$Recovered), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = mapping$Recovered, 
                  position = "bottomright")
      
    }
    
    
      
     
  }) # End renderLeaflet
  
  output$map_anova <- renderTable({
    validate(need(
      is.numeric(ctracking2[[input$mapvar]]) == "TRUE" & input$mapvar != "Month",
      "Please select a numeric variable"
    ))
      t.test(ctracking2[[input$mapvar]] ~ Party, data = ctracking2) %>%
        tidy() %>%
        select(`P-value` = p.value, estimate,
               `95% Lower` = conf.low, `95% Upper` = conf.high)
  }) # End renderTable
  
  output$warning <- renderText({
    if (input$mapvar == "On_Ventilator") {
      "CAUTION: Incomplete data for number of COVID cases on Ventilator"
    } else if (input$mapvar == "Recovered") {
      "CAUTION: Incomplete data number of Recovered COVID Patients"
    }
  })
  
  output$intro <- renderText({
    "testing to see if means differ by political party of Governor"
  })
}

shinyApp(ui, server)