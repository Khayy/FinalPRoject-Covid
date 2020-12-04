library(shiny)
library(tidyverse)
library(broom)

# Load Data
death <- read_rds("../data/COVID_Deaths.rds")
ctracking <- read_rds("../data/ctracking.rds")
age_gender <- read_rds("../data/age_gender.rds")
age_gender%>%
  mutate("Not Applicable" = " ") -> age_gender

death%>%
  rename(State = NAME)%>%
  rename(Deaths = "Number of COVID-19 Deaths")%>%
  mutate("Not Applicable" = " ")%>%
  select(State, "Condition Group", "Condition", "Age Group", Deaths, "Not Applicable")-> death




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
                         verbatimTextOutput("anova_1")),
                         mainPanel(plotOutput("anovaPlot_1")))),
              tabPanel("Statistical Analysis: Individuals With A Pre-existing Condition",
                                sidebarLayout(sidebarPanel(
                                  radioButtons("type_2","Would you like to conduct a one-way or two-way ANOVA?", choices = c("One-way", "Two-way")),
                                  varSelectInput("groups_2.1",label = "What grouping variable would you like to investigate?",
                                                 data = death,
                                                 selected = "Age group"),
                                  varSelectInput("groups_2.2",label = "If you would like to conduct a two-way ANOVA, please select another variable to investigate.",
                                                 data = death,
                                                 selected = "Not Applicable"),
                                  verbatimTextOutput("anova_2")),
                                  mainPanel(plotOutput("anovaPlot_2"))))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
   output$anova_1 <- renderPrint({
    if (!!input$type_1 == "One-way"){
      one.way <- aov(Deaths ~ age_gender[[input$groups_1.1]], data = age_gender)
      print (summary(one.way))
    }
    else if (!!input$type_1 == "Two-way"){
      two.way <- aov(Deaths ~ age_gender[[input$groups_1.1]] + age_gender[[input$groups_1.2]], data = age_gender)
      print (summary(two.way))
    }})
   
  output$anova_2 <- renderPrint({ 
     if (!!input$type_2 == "One-way"){
       two.way <- aov(Deaths ~ death[[input$groups_2.1]], data = death)
       print (summary(two.way))
     }
     else if (!!input$type_2 == "Two-way"){
       two.way <- aov(Deaths ~ death[[input$groups_2.1]] + death[[input$groups_2.2]], data = death)
       print (summary(two.way))
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
       theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 1))
     
     }else if (!!input$type_1 == "Two-way"){
     age_gender%>%
       group_by(!!input$groups_1.1, !!input$groups_1.2)%>%
       ggplot(aes(!!input$groups_1.1, Deaths, fill = !!input$groups_1.2))+
       geom_boxplot(show.legend = FALSE)+
         scale_y_log10()+
         ylab("Log of COVID-19 Deaths")+
         theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 1))
       }
  
 })
 
 output$anovaPlot_2 <- renderPlot({
   if (!!input$type_2 == "One-way"){
     death%>%
       group_by(!!input$groups_2.1)%>%
       ggplot(aes(!!input$groups_2.1, Deaths, fill = !!input$groups_2.1))+
       geom_boxplot(show.legend = FALSE)+ 
       scale_y_log10()+
       ylab("Log of COVID-19 Deaths")+
       theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 1))
     
   }else if (!!input$type_2 == "Two-way"){
     death%>%
       group_by(!!input$groups_2.1, !!input$groups_2.2)%>%
       ggplot(aes(!!input$groups_2.1, Deaths, fill = !!input$groups_2.2))+
       geom_boxplot(show.legend = FALSE)+ 
       scale_y_log10() +
       ylab("Log of COVID-19 Deaths")+
       theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 1))
     }
   
 })
}

# Run the application 
shinyApp(ui = ui, server = server)
