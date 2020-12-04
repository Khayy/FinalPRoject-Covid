library(shiny)
library(tidyverse)
library(broom)

# Load Data
COVID_Deaths <- read_rds("../data/COVID_Deaths.rds")
ctracking <- read_rds("../data/ctracking.rds")
age_gender <- read_rds("../data/age_gender.rds")
pop <- read_rds("../data/pop.rds")

age_gender%>%
  mutate("Not Applicable" = " ") -> age_gender

death %>%
  rename(State = NAME)%>%
  rename(Deaths = "Number of COVID-19 Deaths")%>%
  mutate("Not Applicable" = " ")%>%
  select(State, "Condition Group", "Condition", "Age Group", Deaths, "Not Applicable")-> COVID_Deaths




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Exploring Coronovirus Affects in the United States of America"),
  tabsetPanel(type = "tabs", 
              tabPanel("About"),
              tabPanel ("Data Exploration & Visualization"),
              tabPanel("Statistical Analysis: General Population",
                       sidebarLayout(sidebarPanel(
                         radioButtons("type_1","Would you like to conduct a one-way or two-way ANOVA?", choices = c("One-way", "Two-way")),
                         varSelectInput("groups_1.1",label = "What grouping variable would you like to investigate?",
                                      data = age_gender,
                                      selected = "Age group"),
                         varSelectInput("groups_1.2",label = "If you would like to conduct a two-way ANOVA, please select another variable to investigate.",
                                        data = age_gender,
                                        selected = "Not Applicable"),
                         ),
                         mainPanel(plotOutput("anovaPlot_1"),
                                   tableOutput("anova_1")))), ##changes for output
              tabPanel("Statistical Analysis: Individuals With A Pre-existing Condition",
                                sidebarLayout(sidebarPanel(
                                  radioButtons("type_2","Would you like to conduct a one-way or two-way ANOVA?", choices = c("One-way", "Two-way")),
                                  varSelectInput("groups_2.1",label = "What grouping variable would you like to investigate?",
                                                 data = COVID_Deaths,
                                                 selected = "Age group"),
                                  varSelectInput("groups_2.2",label = "If you would like to conduct a two-way ANOVA, please select another variable to investigate.",
                                                 data = COVID_Deaths,
                                                 selected = "Not Applicable"),
                                  ),
                                  mainPanel(plotOutput("anovaPlot_2"),
                                            tableOutput("anova_2")))) ##changes for output
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
   output$anova_1 <- renderTable({     ##changes for output 
    if (!!input$type_1 == "One-way"){
      aov(Deaths ~ age_gender[[input$groups_1.1]], data = age_gender) %>%
        tidy() %>%
        select(`Term` = term, 
               `Degrees of Freedom` = df, 
               `Sum of Squares` = sumsq, 
               `F Value` = statistic, 
               `P value` = p.value) 
    }
    else if (!!input$type_1 == "Two-way"){   ##changes for output 
    aov(Deaths ~ age_gender[[input$groups_1.1]] + age_gender[[input$groups_1.2]], data = age_gender)%>%
      tidy() %>%
        select(`Term` = term, 
               `Degrees of Freedom` = df, 
               `Sum of Squares` = sumsq, 
               `F Value` = statistic, 
               `P value` = p.value)
    }})
   
  output$anova_2 <- renderTable({        ##changes for output
     if (!!input$type_2 == "One-way"){
      aov(Deaths ~ COVID_Deaths[[input$groups_2.1]], data = COVID_Deaths)%>%
       tidy() %>%
         select(`Term` = term, 
                `Degrees of Freedom` = df, 
                `Sum of Squares` = sumsq, 
                `F Value` = statistic, 
                `P value` = p.value)
     }
     else if (!!input$type_2 == "Two-way"){      ##changes for output
      aov(Deaths ~ COVID_Deaths[[input$groups_2.1]] + COVID_Deaths[[input$groups_2.2]], data = COVID_Deaths)%>%
       tidy() %>%
         select(`Term` = term, 
                `Degrees of Freedom` = df, 
                `Sum of Squares` = sumsq, 
                `F Value` = statistic, 
                `P value` = p.value)
     }
  })
   
 output$anovaPlot_1 <- renderPlot({
   if (!!input$type_1 == "One-way"){
   age_gender%>%
     group_by(!!input$groups_1.1)%>%
     ggplot(aes(!!input$groups_1.1, Deaths, fill = !!input$groups_1.1))+
     geom_boxplot(show.legend = FALSE)+
       scale_y_log10()+
       ylab("Log of COVID-19 Deaths")+
       coord_flip() 
     
     }else if (!!input$type_1 == "Two-way"){
     age_gender%>%
       group_by(!!input$groups_1.1, !!input$groups_1.2)%>%
       ggplot(aes(!!input$groups_1.1, Deaths, fill = !!input$groups_1.2))+
       geom_boxplot(show.legend = FALSE)+
         scale_y_log10()+
         ylab("Log of COVID-19 Deaths")+
         coord_flip() 
       }
  
 })
 
 output$anovaPlot_2 <- renderPlot({
   if (!!input$type_2 == "One-way"){
     COVID_Deaths%>%
       group_by(!!input$groups_2.1)%>%
       ggplot(aes(!!input$groups_2.1, Deaths, fill = !!input$groups_2.1))+
       geom_boxplot(show.legend = FALSE)+ 
       scale_y_log10()+
       ylab("Log of COVID-19 Deaths")+
       coord_flip() 
     
   }else if (!!input$type_2 == "Two-way"){
     COVID_Deaths%>%
       group_by(!!input$groups_2.1, !!input$groups_2.2)%>%
       ggplot(aes(!!input$groups_2.1, Deaths, fill = !!input$groups_2.2))+
       geom_boxplot(show.legend = FALSE)+ 
       scale_y_log10() +
       ylab("Log of COVID-19 Deaths")+
       coord_flip() 
     }
   
 })
}

# Run the application 
shinyApp(ui = ui, server = server)
