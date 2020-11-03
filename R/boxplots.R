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
b <- ctracking %>%
    select(state, death, onVentilatorCumulative, positive) %>%
    filter(state != "AS", state != "GU", state != "MP", state != "VI")
names(b)[3] <- "Number_of_Deaths"
names(b)[4] <- "On_Ventilator"
names(b)[5] <- "Positive_Test"
    
#Get rid of rows whose NAME = NA
bplot <- bplot %>%
    filter(is.na(Name) != "TRUE")

ui <- fluidPage(
  
    fluidRow(title = "Inputs",
        column(4,
            varSelectInput("boxvar1", "Please a Variable", data = bplot,
                           selected = "state"),
            varSelectInput("boxvar2", "Please another Variable", data = bplot,
                           selected = "Number_of_Deaths"),
            radioButtons("outlier", "Do you want to include outliers?",
                         choices = c("Yes", "No"))
        ), # End column
        column(8)
    ), # end fluidRow
    fluidRow(title = "Outputs",
        column(12,
            plotOutput("boxplot", height = 700)
        ) # end mainPanel
    ) # end fluidRow
    
)

server <- function(input, output, session) {
  
    output$boxplot <- renderPlot({
        validate(need(input$boxvar1 != "Month" & input$boxvar2 != "Month", 
                      "Please select a different variable"))
        validate(need(input$boxvar1 != input$boxvar2, 
                      "Please select two different variables"))
        
        a <- bplot %>%
            select(Month, input$boxvar1, input$boxvar2)

            if (input$boxvar1 == "state" | input$boxvar2 == "state") {
                # Make it so that state can be either input
                if (input$boxvar1 == "state") {
                    names(a)[2] <- "b"
                    names(a)[3] <- "c"
                } else {
                    names(a)[3] <- "b"
                    names(a)[2] <- "c"
                }
                # take into account whether to include outliers
                if (input$outlier == "Yes") {
                    p1 <- ggplot(a, mapping = aes(x = b,
                                                  y = c)) +
                        geom_boxplot()+
                        xlab("") +
                        ylab("") +
                        coord_flip()
                } else if (input$outlier == "No") {
                    p1 <- ggplot(a, mapping = aes(x = b,
                                                  y = c)) +
                        geom_boxplot(outlier.shape = NA)+
                        xlab("") +
                        ylab("") +
                        coord_flip()
                }
            } # end state conditions
            else if (input$boxvar1 == "Age_Group" | input$boxvar2 == "Age_Group") {
                a <- a %>%
                    filter(Age_Group != "All Ages", Age_Group != "Not stated")
                
                # Make it so that age can be either input
                if (input$boxvar1 == "Age_Group") {
                    names(a)[2] <- "b"
                    names(a)[3] <- "c"
                } else {
                    names(a)[3] <- "b"
                    names(a)[2] <- "c"
                }
                
                # take into account whether to include outliers
                if (input$outlier == "Yes") {
                    p1 <- ggplot(a, mapping = aes(x = b,
                                                  y = c)) +
                        geom_boxplot()+
                        xlab("") +
                        ylab("") +
                        coord_flip()
                } else if (input$outlier == "No") {
                    p1 <- ggplot(a, mapping = aes(x = b,
                                                  y = c)) +
                        geom_boxplot(outlier.shape = NA)+
                        xlab("") +
                        ylab("") +
                        coord_flip()
                }
            } # end age conditions
           else if (input$boxvar1 == "Condition" | input$boxvar2 == "Condition") {
               a <- a %>%
                   filter(Condition != "COVID-19", 
                          Condition != "All other conditions and causes (residual)")
               # Make it so that age can be either input
               if (input$boxvar1 == "Condition") {
                   names(a)[2] <- "b"
                   names(a)[3] <- "c"
               } else {
                   names(a)[3] <- "b"
                   names(a)[2] <- "c"
               }
               # take into account whether to include outliers
               if (input$outlier == "Yes") {
                   p1 <- ggplot(a, mapping = aes(x = b,
                                                 y = c)) +
                       geom_boxplot()+
                       xlab("") +
                       ylab("") +
                       coord_flip()
               } 
               else if (input$outlier == "No") {
                   p1 <- ggplot(a, mapping = aes(x = b,
                                                 y = c)) +
                       geom_boxplot(outlier.shape = NA)+
                       xlab("") +
                       ylab("") +
                       coord_flip()
               }
           } # end Codition workings
        
           else if (is.numeric(a[[input$boxvar1]]) & is.numeric(a[[input$boxvar1]])) {
               
               p1 <- ggplot(b, mapping = aes(x = !!input$boxvar1, 
                                             y = !!input$boxvar2)) +
                   geom_point()
           } # end both numeric condition
        
            p1 +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1))
            
            p1
    })
    
}

shinyApp(ui, server)