library(shiny)
library(tidyverse)
library(broom)


# Load Data
death <- read_rds("../data/COVID_Deaths.rds")
ctracking <- read_rds("../data/ctracking.rds")
age <- read_rds("../data/age_gender.rds")

# filter death data
death1 <- death %>%
  filter(`Age Group` != "All Ages" & `Age Group` != "Not stated" &
           `Condition Group` != "COVID-19" & Condition != "COVID-19")

# group the ctracking data
ctracking1 <- ctracking %>%
  select(state, positiveIncrease, onVentilatorCurrently) %>%
  group_by(state) %>%
  summarise(Positive_Test = sum(positiveIncrease, na.rm = T),
            On_Ventilator = sum(onVentilatorCurrently, na.rm = T))

# combine data sets
bplot <- inner_join(ctracking1, death1, by = c("state" = "abb"))

#Select the important Variables
bplot <- bplot %>%
    select(state, NAME, `Age Group`, Condition, 
           `Number of COVID-19 Deaths`, Positive_Test, On_Ventilator) %>%
    rename(Age_Group = `Age Group`,
           Number_of_Deaths = `Number of COVID-19 Deaths`,
           Name = NAME)

b <- ctracking %>%
    select(state, deathIncrease, onVentilatorCumulative, positiveIncrease) %>%
    filter(state != "AS", state != "GU", state != "MP", state != "VI")
names(b)[3] <- "Number_of_Deaths"
names(b)[4] <- "On_Ventilator"
names(b)[5] <- "Positive_Test"

# group age by sex
age2 <- age %>%
  group_by(State, Sex) %>%
  summarize(Deaths = sum(Deaths, na.rm = T))

# combine original datasets to get distribution for positive tests and ventilators
bplot2 <- inner_join(ctracking, death, by = c("state" = "abb"))
bplot2 <- inner_join(bplot2, age2, by = c("NAME" = "State"))

bplot2 <- bplot2 %>%
  select(Month, state, NAME, `Age Group`, Condition, Sex, `Number of COVID-19 Deaths`,
         positiveIncrease, onVentilatorCurrently) %>%
  rename(Age_Group = `Age Group`,
         Number_of_Deaths = `Number of COVID-19 Deaths`,
         Positive_Test = positiveIncrease,
         On_Ventilator = onVentilatorCurrently,
         Name = NAME) %>%
  mutate(Month = recode(Month, 
                        `2` = "Feb",
                        `3` = "Mar",
                        `4` = "Apr",
                        `5` = "May",
                        `6` = "Jun",
                        `7` = "Jul",
                        `8` = "Aug",
                        `9` = "Sept",
                        `10` = "Oct")) %>%
  filter(is.na(Name) != "TRUE") %>%
  select(-Name) %>%
  mutate(Condition = recode(Condition,
                            `Intentional and unintentional injury, poisoning, and other adverse events` = "Other adverse events"))


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
    select(-Name) %>%
    mutate(Condition = recode(Condition,
                              `Intentional and unintentional injury, poisoning, and other adverse events` = "Other adverse events"))

ui <- fluidPage(
  
    fluidRow(title = "Inputs",
        column(4,
            varSelectInput("boxvar1", "Please a Variable", data = bplot2,
                           selected = "Month"),
            varSelectInput("boxvar2", "Please another Variable", data = bplot2,
                           selected = "Number_of_Deaths")
        ), # End column
        column(4,
               checkboxInput("outlier", "Do you want to include outliers (Only 
                                        Applicable when selecting one categorical variable
                                        and one numeric variable)?", 
                             value = TRUE)
               ),# end Column
        column(4,
               textOutput("warning"))
    ), # end fluidRow
    fluidRow(title = "Outputs",
        column(6,
            plotOutput("boxplot", height = 500)
        ), # end column
        column(6,
               plotOutput("barplot", height = 500)
        ) #end Column
    ), # end fluidRow
    fluidRow(title = "Overall Death",
             plotOutput("death", height = 500)),
    fluidRow(title = "Overall cases",
             plotOutput("cases", height = 500)),
    fluidRow(title = "Overall Ventilator",
             plotOutput("vent", height = 500))
    
    
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
        
        validate(need(input$boxvar1 == "Month" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "state" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "Age_Group" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "Condition" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "Sex" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "Month" & input$boxvar2 == "Positive_Test" |
                        input$boxvar1 == "Month" & input$boxvar2 == "On_Ventilator" |
                        input$boxvar1 == "state" & input$boxvar2 == "Positive_Test" |
                        input$boxvar1 == "state" & input$boxvar2 == "On_Ventilator" |
                        input$boxvar1 == "Number_of_Deaths" & input$boxvar2 == "Positive_Test" |
                        input$boxvar1 == "Number_of_Deaths" & input$boxvar2 == "On_Ventilator" |
                        input$boxvar1 == "Positive_Test" & input$boxvar2 == "On_Ventilator" |
                        input$boxvar1 == "Positive_Test" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "On_Ventilator" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "On_Ventilator" & input$boxvar2 == "Positive_Test" |
                        input$boxvar2 == "Month" & input$boxvar1 == "Number_of_Deaths" |
                        input$boxvar2 == "state" & input$boxvar1 == "Number_of_Deaths" |
                        input$boxvar2 == "Age_Group" & input$boxvar1 == "Number_of_Deaths" |
                        input$boxvar2 == "Condition" & input$boxvar1 == "Number_of_Deaths" |
                        input$boxvar2 == "Sex" & input$boxvar1 == "Number_of_Deaths" |
                        input$boxvar2 == "Month" & input$boxvar1 == "Positive_Test" |
                        input$boxvar2 == "Month" & input$boxvar1 == "On_Ventilator" |
                        input$boxvar2 == "state" & input$boxvar1 == "Positive_Test" |
                        input$boxvar2 == "state" & input$boxvar1 == "On_Ventilator",
                      "Cannot create graphs for selected variables"))
        

        if (input$boxvar1 == "Sex" | input$boxvar2 == "Sex") {
          a <- age2 %>%
            select(Sex, Deaths)
        }
        else if (input$boxvar1 == "Number_of_Deaths" | input$boxvar2 == "Number_of_Deaths") {
          if (input$boxvar1 == "Month" | input$boxvar2 == "Month") {
            a <- bplot2 %>%
              select(Month, input$boxvar1, input$boxvar2)
          } else {
            a <- bplot %>%
              select(input$boxvar1, input$boxvar2)
          }
        } else {
          a <- bplot2 %>%
            select(Month, input$boxvar1, input$boxvar2)
        }
        
        

            if (input$boxvar1 == "state" | input$boxvar2 == "state") {
                # Make it so that state can be either input
              if (input$boxvar1 == "Number_of_Deaths" | input$boxvar2 == "Number_of_Deaths") {
                if (input$boxvar1 == "state") {
                  names(a)[1] <- "b"
                  names(a)[2] <- "c"
                } else {
                  names(a)[2] <- "b"
                  names(a)[1] <- "c"
                }
              } else {
                if (input$boxvar1 == "state") {
                  names(a)[2] <- "b"
                  names(a)[3] <- "c"
                } else {
                  names(a)[3] <- "b"
                  names(a)[2] <- "c"
                }
              }
                
                # take into account whether to include outliers
                if (input$outlier == TRUE) {
                    p1 <- ggplot(a, mapping = aes(x = b,
                                                  y = c, fill = b)) +
                        geom_boxplot(show.legend = F)+
                        xlab("") +
                        ylab("") +
                        coord_flip()+
                        theme_bw()
                    
                }  else if (input$outlier == FALSE) {
                    p1 <- ggplot(a, mapping = aes(x = b,
                                                  y = c, fill = b)) +
                      geom_boxplot(show.legend = F)+
                        xlab("") +
                        ylab("") +
                        scale_y_log10() +
                        coord_flip()+
                        theme_bw()
                }
            } # end state conditions
         if (input$boxvar1 == "Sex" | input$boxvar2 == "Sex") {
          # Make it so that state can be either input
          
          names(a)[2] <- "b"
          names(a)[3] <- "c"
          
          # take into account whether to include outliers
          if (input$outlier == TRUE) {
            p1 <- ggplot(a, mapping = aes(x = b,
                                          y = c, fill = b)) +
              geom_boxplot(show.legend = F)+
              xlab("") +
              ylab("") +
              coord_flip()+
              theme_bw()
            
          }  else if (input$outlier == FALSE) {
            p1 <- ggplot(a, mapping = aes(x = b,
                                          y = c, fill = b)) +
              geom_boxplot(show.legend = F)+
              xlab("") +
              ylab("") +
              scale_y_log10() +
              coord_flip()+
              theme_bw()
          }
        } # end sex conditions
        if (input$boxvar1 == "Month" | input$boxvar2 == "Month") {
          
          
          names(a)[1] <- "b"
          names(a)[2] <- "c"
          
          a$b <- as.character(a$b)
          a$b <- factor(a$b, levels = c("Feb",  "Mar",  "Apr",  "May",
                                        "Jun",  "Jul",  "Aug",  "Sept", "Oct"))
          
          # take into account whether to include outliers
          if (input$outlier == TRUE) {
            p1 <- ggplot(a, mapping = aes(x = b,
                                          y = c, fill = b)) +
              geom_boxplot(show.legend = F)+
              xlab("") +
              ylab("") +
              coord_flip()+
              theme_bw()
          } 
          else if (input$outlier == FALSE) {
            p1 <- ggplot(a, mapping = aes(x = b,
                                          y = c, fill = b)) +
              geom_boxplot(show.legend = F)+
              xlab("") +
              ylab("") +
              scale_y_log10() +
              coord_flip()+
              theme_bw()
          }
          
        }# End Month Condition
             if (input$boxvar1 == "Age_Group" | input$boxvar2 == "Age_Group") {
                a <- a %>%
                    filter(Age_Group != "All Ages", Age_Group != "Not stated")
                
                # Make it so that age can be either input
                if (input$boxvar1 == "Number_of_Deaths" | input$boxvar2 == "Number_of_Deaths") {
                  if (input$boxvar1 == "Age_Group") {
                    names(a)[1] <- "b"
                    names(a)[2] <- "c"
                  } else {
                    names(a)[2] <- "b"
                    names(a)[1] <- "c"
                  }
                } else {
                  if (input$boxvar1 == "Age_Group") {
                    names(a)[2] <- "b"
                    names(a)[3] <- "c"
                  } else {
                    names(a)[3] <- "b"
                    names(a)[2] <- "c"
                  }
                }
                
                # take into account whether to include outliers
                if (input$outlier == TRUE) {
                    p1 <- ggplot(a, mapping = aes(x = b,
                                                  y = c, fill = b)) +
                      geom_boxplot(show.legend = F)+
                        xlab("") +
                        ylab("") +
                        coord_flip()+
                        theme_bw()
                    
                } else if (input$outlier == FALSE) {
                  p1 <- ggplot(a, mapping = aes(x = b,
                                                y = c, fill = b)) +
                    geom_boxplot(show.legend = F)+
                    xlab("") +
                    ylab("") +
                    scale_y_log10() +
                    coord_flip()+
                    theme_bw()
                }
            } # end age conditions
            if (input$boxvar1 == "Condition" | input$boxvar2 == "Condition") {
               a <- a %>%
                   filter(Condition != "COVID-19", 
                          Condition != "All other conditions and causes (residual)")
               # Make it so that age can be either input
               if (input$boxvar1 == "Number_of_Deaths" | input$boxvar2 == "Number_of_Deaths") {
                 if (input$boxvar1 == "Condition") {
                   names(a)[1] <- "b"
                   names(a)[2] <- "c"
                 } else {
                   names(a)[2] <- "b"
                   names(a)[1] <- "c"
                 }
               } else {
                 if (input$boxvar1 == "Condition") {
                   names(a)[2] <- "b"
                   names(a)[3] <- "c"
                 } else {
                   names(a)[3] <- "b"
                   names(a)[2] <- "c"
                 }
               }
               # take into account whether to include outliers
               if (input$outlier == TRUE) {
                   p1 <- ggplot(a, mapping = aes(x = b,
                                                 y = c, fill = b)) +
                     geom_boxplot(show.legend = F)+
                       xlab("") +
                       ylab("") +
                       coord_flip()+
                       theme_bw()
               } 
               else if (input$outlier == FALSE) {
                 p1 <- ggplot(a, mapping = aes(x = b,
                                               y = c, fill = b)) +
                   geom_boxplot(show.legend = F)+
                   xlab("") +
                   ylab("") +
                   scale_y_log10() +
                   coord_flip()+
                   theme_bw()
               }
           } # end Codition workings
        
            if (is.numeric(a[[input$boxvar1]]) & is.numeric(a[[input$boxvar1]])) {
               
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
        
          
        validate(need(input$boxvar1 == "Month" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "state" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "Age_Group" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "Condition" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "Sex" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "Month" & input$boxvar2 == "Positive_Test" |
                        input$boxvar1 == "Month" & input$boxvar2 == "On_Ventilator" |
                        input$boxvar1 == "state" & input$boxvar2 == "Positive_Test" |
                        input$boxvar1 == "state" & input$boxvar2 == "On_Ventilator" |
                        input$boxvar1 == "Number_of_Deaths" & input$boxvar2 == "Positive_Test" |
                        input$boxvar1 == "Number_of_Deaths" & input$boxvar2 == "On_Ventilator" |
                        input$boxvar1 == "Positive_Test" & input$boxvar2 == "On_Ventilator" |
                        input$boxvar1 == "Positive_Test" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "On_Ventilator" & input$boxvar2 == "Number_of_Deaths" |
                        input$boxvar1 == "On_Ventilator" & input$boxvar2 == "Positive_Test" |
                        input$boxvar2 == "Month" & input$boxvar1 == "Number_of_Deaths" |
                        input$boxvar2 == "state" & input$boxvar1 == "Number_of_Deaths" |
                        input$boxvar2 == "Age_Group" & input$boxvar1 == "Number_of_Deaths" |
                        input$boxvar2 == "Condition" & input$boxvar1 == "Number_of_Deaths" |
                        input$boxvar2 == "Sex" & input$boxvar1 == "Number_of_Deaths" |
                        input$boxvar2 == "Month" & input$boxvar1 == "Positive_Test" |
                        input$boxvar2 == "Month" & input$boxvar1 == "On_Ventilator" |
                        input$boxvar2 == "state" & input$boxvar1 == "Positive_Test" |
                        input$boxvar2 == "state" & input$boxvar1 == "On_Ventilator",
                      "Cannot create graphs for selected variables"))
            
        if (input$boxvar1 == "Sex" | input$boxvar2 == "Sex") {
          a <- age2 %>%
            select(Sex, Deaths)
        }
        else if (input$boxvar1 == "Number_of_Deaths" | input$boxvar2 == "Number_of_Deaths") {
          if (input$boxvar1 == "Month" | input$boxvar2 == "Month") {
            a <- bplot2 %>%
              select(Month, input$boxvar1, input$boxvar2)
          } else {
            a <- bplot %>%
              select(input$boxvar1, input$boxvar2)
          }
        } else {
          a <- bplot2 %>%
            select(Month, input$boxvar1, input$boxvar2)
        }
            
            if (input$boxvar1 == "state" | input$boxvar2 == "state") {
                # Make it so that state can be either input
              if (input$boxvar1 == "Number_of_Deaths" | input$boxvar2 == "Number_of_Deaths") {
                if (input$boxvar1 == "state") {
                  names(a)[1] <- "b"
                  names(a)[2] <- "c"
                } else {
                  names(a)[2] <- "b"
                  names(a)[1] <- "c"
                }
              } else {
                if (input$boxvar1 == "state") {
                  names(a)[2] <- "b"
                  names(a)[3] <- "c"
                } else {
                  names(a)[3] <- "b"
                  names(a)[2] <- "c"
                }
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
        
         if (input$boxvar1 == "Sex" | input$boxvar2 == "Sex") {
          # Make it so that state can be either input
           

          names(a)[2] <- "b"
          names(a)[3] <- "c"
          
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
        } # end sex conditions
        
            else if (input$boxvar1 == "Age_Group" | input$boxvar2 == "Age_Group") {
                a <- a %>%
                    filter(Age_Group != "All Ages", Age_Group != "Not stated")
                # Make it so that state can be either input
                if (input$boxvar1 == "Number_of_Deaths" | input$boxvar2 == "Number_of_Deaths") {
                  if (input$boxvar1 == "Age_Group") {
                    names(a)[1] <- "b"
                    names(a)[2] <- "c"
                  } else {
                    names(a)[2] <- "b"
                    names(a)[1] <- "c"
                  }
                } else {
                  if (input$boxvar1 == "Age_Group") {
                    names(a)[2] <- "b"
                    names(a)[3] <- "c"
                  } else {
                    names(a)[3] <- "b"
                    names(a)[2] <- "c"
                  }
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
                if (input$boxvar1 == "Number_of_Deaths" | input$boxvar2 == "Number_of_Deaths") {
                  if (input$boxvar1 == "Condition") {
                    names(a)[1] <- "b"
                    names(a)[2] <- "c"
                  } else {
                    names(a)[2] <- "b"
                    names(a)[1] <- "c"
                  }
                } else {
                  if (input$boxvar1 == "Condition") {
                    names(a)[2] <- "b"
                    names(a)[3] <- "c"
                  } else {
                    names(a)[3] <- "b"
                    names(a)[2] <- "c"
                  }
                }
                a <- a %>%
                    group_by(b) %>%
                    summarise(sum(c, na.rm = T))
                
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
                a$b <- factor(a$b, levels = c("Feb",  "Mar",  "Apr",  "May",
                                              "Jun",  "Jul",  "Aug",  "Sept", "Oct"))
                
                p2 <- ggplot(a, mapping = aes(x = b, y = c)) +
                    geom_col() +
                    theme_bw() +
                    xlab("")+
                    ylab("") +
                    theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1))
            } # end Month conditions
            
            
            p2
        })
    
    output$warning <- renderText({
      if (input$boxvar1 == "On_Ventilator" | input$boxvar2 == "On_Ventilator") {
        "CAUTION: Incomplete data number of COVID patients on Ventilators"
      }
    })
    
    output$death <- renderPlot({
      ggplot(data = bplot2, aes(x = Number_of_Deaths)) +
        geom_histogram()+
        xlab("Deaths") +
        ylab("") +
        ggtitle("Distribution of Deaths") 
    })
    
    output$cases <- renderPlot({
      ggplot(data = bplot2, aes(x = Positive_Test)) +
        geom_histogram()+
        xlab("Positive Tests") +
        ylab("") +
        ggtitle("Distribution of Positive Tests") 
    })
    
    output$vent <- renderPlot({
      ggplot(data = bplot2, aes(x = On_Ventilator)) +
        geom_histogram()+
        xlab("People on Ventilators") +
        ylab("") +
        ggtitle("Distribution of People on Ventilators",
                subtitle = "CAUTION: Incomplete data number of COVID patients on Ventilators") 
    })
}

shinyApp(ui, server)