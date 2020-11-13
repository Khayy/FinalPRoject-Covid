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

b <- b %>%
    mutate(Month = recode(Month, 
                          `2` = "Feb",
                          `3` = "Mar",
                          `4` = "Apr",
                          `5` = "May",
                          `6` = "Jun",
                          `7` = "Jul",
                          `8` = "Aug",
                          `9` = "Sept",
                          `10` = "Oct"))
    
#Get rid of rows whose NAME = NA
bplot <- bplot %>%
    filter(is.na(Name) != "TRUE") %>%
    select(-Name)

# Recode month
bplot <- bplot %>%
    mutate(Month = recode(Month, 
                      `2` = "Feb",
                      `3` = "Mar",
                      `4` = "Apr",
                      `5` = "May",
                      `6` = "Jun",
                      `7` = "Jul",
                      `8` = "Aug",
                      `9` = "Sept",
                      `10` = "Oct"),
           Condition = recode(Condition,
                             `Intentional and unintentional injury, poisoning, and other adverse events` = "Other adverse events"))

ui <- fluidPage(
  
    fluidRow(title = "Inputs",
        column(4,
            varSelectInput("boxvar1", "Please a Variable", data = bplot,
                           selected = "state"),
            varSelectInput("boxvar2", "Please another Variable", data = bplot,
                           selected = "Number_of_Deaths")
        ), # End column
        column(4,
               checkboxInput("outlier", "Do you want to include outliers (Only 
                                        Applicable when selecting one categorical variable
                                        and one numeric variable)?", 
                             value = TRUE),
               numericInput("ylim", "Please select an uper limit for the graph scale 
                                     (ONLY APPLICABLE WHEN NOT INCLUDING OUTLIERS). 
                                     If graph is hard to understand, leave this 
                                     box blank",
                           min = 100, max = 1000000000, value = 20000000, step = 1)
               ),# end Column
        column(4)
    ), # end fluidRow
    fluidRow(title = "Outputs",
        column(6,
            plotOutput("boxplot", height = 500)
        ), # end column
        column(6,
               plotOutput("barplot", height = 500)
        ) #end Column
    ) # end fluidRow
    
)

server <- function(input, output, session) {
  
    # Two unique variables
    output$boxplot <- renderPlot({
        validate(need(input$boxvar1 != input$boxvar2, 
                      "Please select two unique variables"))
        
        # Have at least one numeric variable
        validate(need(is.numeric(bplot[[input$boxvar1]]) | 
                          is.numeric(bplot[[input$boxvar2]]),
                      "Please select at least one numeric variable"))
        
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
                if (input$outlier == TRUE) {
                    p1 <- ggplot(a, mapping = aes(x = b,
                                                  y = c)) +
                        geom_boxplot()+
                        xlab("") +
                        ylab("") +
                        coord_flip()+
                        theme_bw()
                    
                } else if (input$outlier == FALSE) {
                    p1 <- ggplot(a, mapping = aes(x = b,
                                                  y = c)) +
                        geom_boxplot(outlier.shape = NA)+
                        xlab("") +
                        ylab("") +
                        ylim(0, input$ylim) +
                        coord_flip()+
                        theme_bw()
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
                if (input$outlier == TRUE) {
                    p1 <- ggplot(a, mapping = aes(x = b,
                                                  y = c)) +
                        geom_boxplot()+
                        xlab("") +
                        ylab("") +
                        coord_flip()+
                        theme_bw()
                    
                } else if (input$outlier == FALSE) {
                    p1 <- ggplot(a, mapping = aes(x = b,
                                                  y = c)) +
                        geom_boxplot(outlier.shape = NA)+
                        xlab("") +
                        ylab("") +
                        ylim(0, input$ylim) +
                        coord_flip()+
                        theme_bw()
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
               if (input$outlier == TRUE) {
                   p1 <- ggplot(a, mapping = aes(x = b,
                                                 y = c)) +
                       geom_boxplot()+
                       xlab("") +
                       ylab("") +
                       coord_flip()+
                       theme_bw()
               } 
               else if (input$outlier == FALSE) {
                   p1 <- ggplot(a, mapping = aes(x = b,
                                                 y = c)) +
                       geom_boxplot(outlier.shape = NA)+
                       xlab("") +
                       ylab("") +
                       ylim(0, input$ylim) +
                       coord_flip()+
                       theme_bw()
               }
           } # end Codition workings
           else if (input$boxvar1 == "Month" | input$boxvar2 == "Month") {
               
               if (input$boxvar1 == "Month") {
                   a <- b %>%
                       select(Month, !!input$boxvar2)
               }
               else {
                   a <- b %>%
                       select(Month, !!input$boxvar1)
               }

                   names(a)[1] <- "b"
                   names(a)[2] <- "c"
               
               # take into account whether to include outliers
               if (input$outlier == TRUE) {
                   p1 <- ggplot(a, mapping = aes(x = b,
                                                 y = c)) +
                       geom_boxplot() +
                       xlab("") +
                       ylab("") +
                       coord_flip()+
                       theme_bw()
               } 
               else if (input$outlier == FALSE) {
                   p1 <- ggplot(a, mapping = aes(x = b,
                                                 y = c)) +
                       geom_boxplot(outlier.shape = NA) +
                       xlab("") +
                       ylab("") +
                       ylim(0, input$ylim) +
                       coord_flip()+
                       theme_bw()
               }
               
           }# End Month Condition
        
           else if (is.numeric(a[[input$boxvar1]]) & is.numeric(a[[input$boxvar1]])) {
               
               p1 <- ggplot(b, mapping = aes(x = !!input$boxvar1, 
                                             y = !!input$boxvar2)) +
                   geom_point() +
                   theme_bw()
           } # end both numeric condition

            p1
            
            })
    output$barplot <- renderPlot({
        # Two unique variables
        validate(need(input$boxvar1 != input$boxvar2, 
                          "Please select two unique variables"))
            
        # Have at least one numeric variable
        validate(need(is.numeric(bplot[[input$boxvar1]]) | 
                              is.numeric(bplot[[input$boxvar2]]),
                          "Please select at least one numeric variable"))
        # only one numeric
        validate(need(is.numeric(bplot[[input$boxvar1]]) == "TRUE" & 
                          is.numeric(bplot[[input$boxvar2]]) == "FALSE" |
                          is.numeric(bplot[[input$boxvar1]]) == "FALSE" & 
                          is.numeric(bplot[[input$boxvar2]]) == "TRUE",
                      "Cannot be done when there is not a categorical variable"))
            
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
                a <- a %>%
                    group_by(b) %>%
                    summarize(sum(c, na.rm = T))
                names(a)[2] <- "c"
                p2 <- ggplot(a, mapping = aes(x = reorder(b, -c),
                                              y = c)) +
                    geom_col() +
                    theme_bw() +
                    xlab("")+
                    ylab("") +
                    theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1))
            } # end state conditions
            else if (input$boxvar1 == "Age_Group" | input$boxvar2 == "Age_Group") {
                a <- a %>%
                    filter(Age_Group != "All Ages", Age_Group != "Not stated")
                # Make it so that state can be either input
                if (input$boxvar1 == "Age_Group") {
                    names(a)[2] <- "b"
                    names(a)[3] <- "c"
                } else {
                    names(a)[3] <- "b"
                    names(a)[2] <- "c"
                }
                a <- a %>%
                    group_by(b) %>%
                    summarize(sum(c, na.rm = T))
                names(a)[2] <- "c"
                p2 <- ggplot(a, mapping = aes(x = reorder(b, -c),
                                              y = c)) +
                    geom_col() +
                    theme_bw() +
                    xlab("")+
                    ylab("") +
                    theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1))
            } # end Age conditions
            
            else if (input$boxvar1 == "Condition" | input$boxvar2 == "Condition") {
                a <- a %>%
                    filter(Condition != "COVID-19", 
                           Condition != "All other conditions and causes (residual)")
                
                # Make it so that state can be either input
                if (input$boxvar1 == "Condition") {
                    names(a)[2] <- "b"
                    names(a)[3] <- "c"
                } else {
                    names(a)[3] <- "b"
                    names(a)[2] <- "c"
                }
                a <- a %>%
                    group_by(b) %>%
                    summarize(sum(c, na.rm = T))
                names(a)[2] <- "c"
                p2 <- ggplot(a, mapping = aes(x = reorder(b, -c),
                                              y = c)) +
                    geom_col() +
                    theme_bw() +
                    xlab("")+
                    ylab("") +
                    theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1))
            } # end Condition conditions
            
            
            else if (input$boxvar1 == "Month" | input$boxvar2 == "Month") {
                if (input$boxvar1 == "Month") {
                    a <- b %>%
                        select(Month, !!input$boxvar2)
                }
                else {
                    a <- b %>%
                        select(Month, !!input$boxvar1)
                }
                
                
                names(a)[1] <- "b"
                names(a)[2] <- "c"
                
                
                a$b <- as.character(a$b)
                
                p2 <- ggplot(a, mapping = aes(x = reorder(b, -c), y = c)) +
                    geom_col() +
                    theme_bw() +
                    xlab("")+
                    ylab("") +
                    theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1))
            } # end Month conditions
            
            
            p2
        })
}

shinyApp(ui, server)