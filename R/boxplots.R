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
    select(state, NAME, `Age Group`, Condition, 
           `Number of COVID-19 Deaths`, positive, onVentilatorCurrently) %>%
    rename(Positive_Test = positive,
           On_Ventilator = onVentilatorCurrently,
           Age_Group = `Age Group`,
           Number_of_Deaths = `Number of COVID-19 Deaths`,
           Name = NAME)

#Get rid of rows whose NAME = NA
bplot <- bplot %>%
    filter(is.na(Name) != "TRUE")

ui <- fluidPage(
  
    fluidRow(title = "Inputs",
        column(4,
            varSelectInput("boxvar1", "Please a Variable", data = bplot,
                           selected = "state"),
            varSelectInput("boxvar2", "Please another Variable", data = bplot,
                           selected = "Number_of_Deaths")
        ), # End column
        column(8)
    ), # end fluidRow
    fluidRow(title = "Outputs",
        column(12,
            plotOutput("boxplot")
        ) # end mainPanel
    ) # end fluidRow
    
)

server <- function(input, output, session) {
  
    output$boxplot <- renderPlot({
        validate(need(input$boxvar1 != "Month" & input$boxvar2 != "Month", 
                      "Please select a different variable"))
        
        a <- bplot %>%
            select(input$boxvar1, input$boxvar2)
        
            if (input$boxvar1 == "state") {
                a <- a %>%
                    group_by(state) %>%
                    summarize(sum(!!input$boxvar2, na.rm = T)) 
                names(a)[2] <- "b"
               p1 <- ggplot(a, mapping = aes(x = reorder(!!input$boxvar1, -b), 
                                        y = b)) +
                    geom_col()
            }
            else if (input$boxvar2 == "state") {
                #FIX THIS
                a <- a %>%
                    group_by(state) %>%
                    summarize(sum(!!input$boxvar1, na.rm = T)) 
                p1 <- ggplot(a, mapping = aes(x = sum(!!input$boxvar1, na.rm = T),
                                              y = !!input$boxvar2)) +
                    geom_boxplot()
            }
        
            p1 +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1))
            
            p1
    })
    
}

shinyApp(ui, server)