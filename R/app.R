## The final App
library(shiny)
library(tidyverse)
library(broom)
library(sf) 
library(shinythemes)
library(fresh)
library(leaflet)
library(viridis)
#library(tigris)

# Load Data
death <- read_rds("../data/COVID_Deaths.rds")
ctracking <- read_rds("../data/ctracking.rds")
age_gender <- read_rds("../data/age_gender.rds")
age_gender%>%
  mutate("Not Applicable" = " ") -> age_gender

death %>%
  rename(State = NAME)%>%
  rename(Deaths = "Number of COVID-19 Deaths")%>%
  mutate("Not Applicable" = " ")%>%
  select(State, "Condition Group", "Condition", "Age Group", Deaths, "Not Applicable")-> COVID_Deaths

death11 <- read_rds("../data/COVID_Deaths.rds")
ctracking11 <- read_rds("../data/ctracking.rds")
age1 <- read_rds("../data/age_gender.rds")

# filter death data
death1 <- death11 %>%
  filter(`Age Group` != "All Ages" & `Age Group` != "Not stated" &
           `Condition Group` != "COVID-19" & Condition != "COVID-19")

# group the ctracking data
ctracking1 <- ctracking11 %>%
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

b <- ctracking11 %>%
  select(state, deathIncrease, onVentilatorCumulative, positiveIncrease) %>%
  filter(state != "AS", state != "GU", state != "MP", state != "VI")
names(b)[3] <- "Number_of_Deaths"
names(b)[4] <- "On_Ventilator"
names(b)[5] <- "Positive_Test"

# group age by sex
age2 <- age1 %>%
  group_by(State, Sex) %>%
  summarize(Deaths = sum(Deaths, na.rm = T))

# combine original datasets to get distribution for positive tests and ventilators
bplot2 <- inner_join(ctracking11, death11, by = c("state" = "abb"))
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



# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  # Application title
  titlePanel("Exploring Coronovirus Affects in the United States of America"),
  tabsetPanel(type = "tabs", 
              tabPanel("About",
                       h2("ABOUT"),
                       p("Explore the effects of coronavirus (COVID-19) at the individual state level with this shiny app. As we near the end of 2020 COVID-19 cases are on the rise again in the United States and understanding the effects by state can help you make informed decisions about your own wellbeing. This app will allow you to visualize and analyze COVID-19 cases and deaths across the United States. You can investigate coronavirus through exploring COVID positive results, progression to extreme medical attention (individuals on ventilators), and death rates through several other factors (age, sex, location by state and pre-existing conditions). Additionally, you will be able to look at the general population statistics for the country and each state to better understand the impact COVID has had on the population. 
"), 
                       h2("DATA"),
                       p("The data on COVID cases and outcomes comes from the website covidtracking. More information about this data can be found at the following link: https://covidtracking.com/about-data. This data was cleaned and formatted for this app. Data includes covid cases and deaths by state since february 2020."),
                       p("Additional data for this app was used to further help visualize and understand the impact of COVID-19 on the United States. Two datasets from the Center for Disease Control (CDC) were used. The first containing COVID deaths by age and gender (1) and the second containing COVID deaths by pre-existing condition and age (2). More information about these datasets can be found at the following links: (1)https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-S/9bhg-hcku 
(2)https://data.cdc.gov/NCHS/Conditions-contributing-to-deaths-involving-corona/hk9y-quqm"), 
                       h2("HOW TO USE THE APP"),
                       p("This app will allow you to investigate COVID-19 in the United States with data collected since February 2020. This app will investigate its impact through three key ways. 
"),
                       p("Data Exploration & Visualization: First you will be able to compare COVID-related deaths and COVID cases to the general population since February of 2020. This will allow you to see the proportion of people who have been diagnosed with COVID-19 in the United States. You will additionally be able to visualize compare confirmed cases and deaths due to COVID-19 by age, by gender, by pre-existing conditions, by month, and by state in the United States. These will be visualized in two ways. First, you will be able to look at boxplots comparing the distribution of COVID-19 deaths for each group in the variable. In addition, you will also be able to look at bar graphs to look at which group has the most COVID-19 cases. You will also be able to visually investigate the relationship of age, existing conditions, month, and location (by state) to total COVID positive test results, extreme progression of the virus (individuals on ventilators), and COVID deaths. You will be able to see these relations through boxplots to get a sense of the distribution, bar graphs to get a sense of the total numbers, and a written summary output generated for each visualization. For each of the boxplots created you will have the choice as to whether include outliers in the graph or not. If you choose to not include outliers, you can change the maximum value of the x-axis to make the graph easier to read. In addition, there are boxplots presented for the overall distribution of COVID-19 deaths, positive test results, and extreme progression of the virus (individuals on ventilators). 
"), ##Need to add specifics from evans app 
                       p("Mapping: You will be able to look at COVID-19 related facts on a map. In this map, you can choose whether to visualize COVID-19 deaths, number of positive test results, or number of negative test results. Once you select which of those three you would like to explore more, a map of the United States will appear, where each state is a different shade of red. The darker the red the state is, the more of that input there is. For example, if you choose \"death\" as what you would like to explore, the darker the red a state is the more deaths there are in that state compared to the other states. It is important to note that these are totals for the entire pandemic, not for specific months. In addition, if you were to click on any state on the map, the name of the state, as well as the number for that state would be presented. On the map, you can choose to zoom in or out, or move around more to focus on specific regions/states as needed. Based on the input you choose for the mapping, different t-tests will be run. They will compare whether the political party of state’s governors affect the number of COVID-19 deaths, positive test results, or negative test results. Whichever you choose to visualize on the map of the United States will be the same variable used in this t-test. The app presents the p-value, estimate, and 95% confidence interval from the t-test.
"), 
                       p("Statistical Analysis: Additional to the visualizations described above, you will have the opportunity to conduct several statistical analyses on the data previously described. Specifically, you will be able to conduct both a one-way and two-way analysis of variance (ANOVA) to investigate if there is a statistically significant difference in COVID outcomes. To learn more about ANOVA’s please follow this link: https://www.sciencedirect.com/topics/medicine-and-dentistry/analysis-of-variance"),
                       p("You will be able to conduct an one-way or two-way ANOVA on two populations. You will have the opportunity to look at the general population and conduct an ANOVA on COVID-19 deaths by age, location, and gender. Additionally, you will have the opportunity to look at the individuals with a pre-existing condition and conduct an ANOVA on COVID-19 deaths by age, condition, condition type and location. Results of an ANOVA analysis will tell you if there is a statistically significant difference between groups in relation to your outcome choice. Visualization for this analysis will also be created through box-plots.
")),
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
                                        data = COVID_Deaths,
                                        selected = "Age group"),
                         varSelectInput("groups_2.2",label = "If you would like to conduct a two-way ANOVA, please select another variable to investigate.",
                                        data = COVID_Deaths,
                                        selected = "Not Applicable"),
                         verbatimTextOutput("anova_2")),
                         mainPanel(plotOutput("anovaPlot_2")))),
              tabPanel("Statistical Analysis: Boxplots",
                       fluidRow(title = "Inputs",
                                column(4,
                                       varSelectInput("boxvar1", "Please a Variable", data = bplot2,
                                                      selected = "Month"),
                                       varSelectInput("boxvar2", "Please another Variable", data = bplot2,
                                                      selected = "Number_of_Deaths")
                                ), # End column
                                column(4,
                                       checkboxInput("outlier", "Perform a log transformation (Only 
                                        Applicable for the boxplot)?", 
                                                     value = FALSE)
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
                                plotOutput("vent", height = 500))),
              tabPanel("Maps Graphics")
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
      two.way <- aov(Deaths ~ COVID_Deaths[[input$groups_2.1]], data = COVID_Deaths)
      print (summary(two.way))
    }
    else if (!!input$type_2 == "Two-way"){
      two.way <- aov(Deaths ~ COVID_Deaths[[input$groups_2.1]] + COVID_Deaths[[input$groups_2.2]], data = COVID_Deaths)
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
      COVID_Deaths%>%
        group_by(!!input$groups_2.1)%>%
        ggplot(aes(!!input$groups_2.1, Deaths, fill = !!input$groups_2.1))+
        geom_boxplot(show.legend = FALSE)+ 
        scale_y_log10()+
        ylab("Log of COVID-19 Deaths")+
        theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 1))
      
    }else if (!!input$type_2 == "Two-way"){
      COVID_Deaths%>%
        group_by(!!input$groups_2.1, !!input$groups_2.2)%>%
        ggplot(aes(!!input$groups_2.1, Deaths, fill = !!input$groups_2.2))+
        geom_boxplot(show.legend = FALSE)+ 
        scale_y_log10() +
        ylab("Log of COVID-19 Deaths")+
        theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 1))
    }
    
  })
  
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
        a <- b %>%
          select(Month, input$boxvar1, input$boxvar2)
      } else {
        a <- bplot %>%
          select(input$boxvar1, input$boxvar2)
      }
    } else {
      a <- bplot2 %>%
        select(Month, input$boxvar1, input$boxvar2)
    }
    
    if (input$boxvar1 == "state" & input$boxvar2 == "On_Ventilator" |
        input$boxvar2 == "state" & input$boxvar1 == "On_Ventilator") {
      
      a2 <- a %>%
        group_by(state) %>%
        summarise(sum = sum(On_Ventilator)) %>%
        filter(sum > 0)
      
      a <- semi_join(a, a2, by = "state")
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
      if (input$outlier == FALSE) {
        p1 <- ggplot(a, mapping = aes(x = b,
                                      y = c, fill = b)) +
          geom_boxplot(show.legend = F)+
          xlab("") +
          ylab("") +
          coord_flip()+
          theme_bw()
        
      }  else if (input$outlier == TRUE) {
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
      if (input$outlier == FALSE) {
        p1 <- ggplot(a, mapping = aes(x = b,
                                      y = c, fill = b)) +
          geom_boxplot(show.legend = F)+
          xlab("") +
          ylab("") +
          coord_flip()+
          theme_bw()
        
      }  else if (input$outlier == TRUE) {
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
      
      a <- a %>%
        filter(!is.na(b))
      
      # take into account whether to include outliers
      if (input$outlier == FALSE) {
        p1 <- ggplot(a, mapping = aes(x = b,
                                      y = c, fill = b)) +
          geom_boxplot(show.legend = F)+
          xlab("") +
          ylab("") +
          coord_flip()+
          theme_bw()
      } 
      else if (input$outlier == TRUE) {
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
      if (input$outlier == FALSE) {
        p1 <- ggplot(a, mapping = aes(x = b,
                                      y = c, fill = b)) +
          geom_boxplot(show.legend = F)+
          xlab("") +
          ylab("") +
          coord_flip()+
          theme_bw()
        
      } else if (input$outlier == TRUE) {
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
      if (input$outlier == FALSE) {
        p1 <- ggplot(a, mapping = aes(x = b,
                                      y = c, fill = b)) +
          geom_boxplot(show.legend = F)+
          xlab("") +
          ylab("") +
          coord_flip()+
          theme_bw()
      } 
      else if (input$outlier == TRUE) {
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
    
    if (input$boxvar1 == "state" & input$boxvar2 == "On_Ventilator" |
        input$boxvar2 == "state" & input$boxvar1 == "On_Ventilator") {
      
      a2 <- a %>%
        group_by(state) %>%
        summarise(sum = sum(On_Ventilator)) %>%
        filter(sum > 0)
      
      a <- semi_join(a, a2, by = "state")
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

# Run the application 
shinyApp(ui = ui, server = server)
