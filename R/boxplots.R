library(shiny)
library(tidyverse)
library(broom)


# Load Data
death <- read_rds("../data/COVID_Deaths.rds")
ctracking <- read_rds("../data/ctracking.rds")

# combine data sets
bplot <- left_join(ctracking, death, by = c("state" = "abb"))

#Select the important Variables
bplot <- bplot %>%
    select(state, NAME, `Age Group`, `Condition Group`, Condition, 
           `Number of COVID-19 Deaths`, positive, onVentilatorCurrently) %>%
    rename(Positive_Test = positive,
           On_Ventilator = onVentilatorCurrently,
           Age_Group = `Age Group`,
           Condition_Group = `Condition Group`,
           Number_of_Deaths = `Number of COVID-19 Deaths`)


ui <- fluidPage(
  
    sidebarLayout(
        sidebarPanel(
            varSelectInput("boxvar1", "Please a Variable", data = bplot,
                           selected = "state"),
            varSelectInput("boxvar2", "Please another Variable", data = bplot,
                           selected = "Number_of_Deaths")
        ), # End sidebarPanel
        
        mainPanel(
            
        ) # end mainPanel
    ) # end sidebarLayout
    
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)