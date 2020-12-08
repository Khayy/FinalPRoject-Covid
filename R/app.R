## The final App
library(shiny)
library(tidyverse)
library(broom)
library(sf) 
library(shinythemes)
library(fresh)
library(leaflet)
library(viridis)
library(tigris)

# Load Data
death <- read_rds("../data/COVID_Deaths.rds")
ctracking <- read_rds("../data/ctracking.rds")
age_gender <- read_rds("../data/age_gender.rds")
pop <- read_rds("../data/pop.rds")
age_gender%>%
  mutate("Not Applicable" = " ") -> age_gender
age_gender%>%  ###removing variables update
  mutate("Not Applicable" = " ") %>%
  filter(`Age group` != c("24-34 years")) %>%
  filter(`Age group` != c("35-44 years")) %>%
  filter(`Age group` != c("45-54 years")) %>%
  filter(`Age group` != c("55-64 years"))%>%
  filter (`State` != "United States")-> age_gender
# proportions
# tidy each table
age <- read_rds("../data/age_gender.rds") 
# tidy each table
death_dd <- death %>% 
  select(abb,NAME,population,`Condition Group`,`Age Group`,`Number of COVID-19 Deaths`) %>% 
  rename(deaths = "Number of COVID-19 Deaths",
         Age_Group = `Age Group`,
         state="abb",
         Condition_Group=`Condition Group`) %>%  
  mutate(Condition_Group=recode(Condition_Group,
                                `All other conditions and causes (residual)`="other",
                                `Intentional and unintentional injury, poisoning, and other adverse events`="adverse events")) %>%
  filter(Age_Group != "All Ages" & Age_Group != "Not stated", NAME != "UStotal")

ctracking_dd <- ctracking %>% 
  select(Month,state,deathIncrease,
         negativeIncrease,positiveIncrease ) %>%
  mutate(Month = recode(Month, 
                        `2` = "Feb",
                        `3` = "Mar",
                        `4` = "Apr",
                        `5` = "May",
                        `6` = "Jun",
                        `7` = "Jul",
                        `8` = "Aug",
                        `9` = "Sept",
                        `10` = "Oct",
                        `11` = "Nov"))


age_dd <- age %>% 
  rename(NAME="State")
age_dd$Deaths[is.na(age_dd$Deaths)]=0



# combine three data  
combine_dd <- death_dd %>% 
  inner_join(ctracking_dd, by = "state") %>% 
  inner_join(age_dd, by = "NAME")


dimension <- c("Age_Group","Condition","Status","COVID19")

death %>%
  rename(State = NAME)%>%
  rename(Deaths = "Number of COVID-19 Deaths")%>%
  mutate("Not Applicable" = " ")%>%
  select(State, "Condition Group", "Condition", "Age Group", Deaths, "Not Applicable")%>%
  filter(`Age Group` != "Not stated") %>%
  filter(Condition != "COVID-19")%>%
  filter(`Condition Group` != "Covid-19")%>%
  filter(State != "UStotal") %>%
  filter(`Age Group` != "All Ages") -> COVID_Deaths

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

# Load Data
parties1 <- read_rds("../data/political_parties_of_states.rds")
ctracking1 <- read_rds("../data/ctracking.rds")
pop1 <- read_rds("../data/pop.rds")

# combine data sets
ctracking2 <- left_join(ctracking1, parties1, by = "state")

#making the New Progressive/Republican Republican
ctracking2$Party[ctracking2$Party == "New Progressive/Republican"] <- "Republican"

# select relevent variables
ctracking2 <- ctracking2 %>%
  select(state,Name, Party, deathIncrease, hospitalizedCumulative,
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
  group_by(state, NAME, Party) %>%
  summarise( 
    Negative_Test = sum(Negative_Test),
    Positive_Test = sum(Positive_Test),
    death = sum(death)) %>%
  select(state, NAME, Party, death, Positive_Test, Negative_Test, everything())

# Get per capita

ctracking2 <- left_join(ctracking2, pop1, by = c("NAME" = "NAME"))
ctracking2 <- ctracking2 %>%
  rename(Population = POPESTIMATE2019)

ctracking2 <- ctracking2 %>%
  mutate(deaths_per_capita = death/Population * 100000,
         positive_test_per_capita = Positive_Test/Population * 100000,
         negative_test_per_capita = Negative_Test/Population * 100000) 

ctracking2 <- ctracking2 %>%
  mutate_at(vars(deaths_per_capita, positive_test_per_capita, negative_test_per_capita),
            funs(round(., digits = 5)))

# make dataset for variable list
ctracking4 <- ctracking2 %>%
  select(-positive_test_per_capita, -deaths_per_capita, 
         -negative_test_per_capita, - Population)

#get the state spacial data
states <- states(cb = T)

# remove observations in state that are not in ctracking2
states <- semi_join(states, ctracking2, by = c("STUSPS" = "state"))

# combine spatial data to normal data
mapping <- geo_join(states, ctracking2, "NAME", "NAME")



# UI 
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("Exploring Coronavirus Effects in the United States of America"),
  tabsetPanel(type = "tabs", 
              tabPanel("About",       
                       h2("ABOUT"),
                       p("Explore the effects of coronavirus (COVID-19) at the individual state level with this shiny app. As we near the end of 2020 COVID-19 cases are on the rise again in the United States and understanding the effects by state can help you make informed decisions about your own wellbeing. This app will allow you to visualize and analyze COVID-19 cases and deaths across the United States. You can investigate coronavirus through exploring COVID positive results, progression to extreme medical attention (individuals on ventilators), and death rates through several other factors (age, sex, location by state and pre-existing conditions). Additionally, you will be able to look at the general population statistics for the country and each state to better understand the impact COVID has had on the population. 
"), 
                       h2("DATA"),
                       p("The data on COVID cases and outcomes comes from the website covidtracking. More information about this data can be found at the following link: https://covidtracking.com/about-data. This data was cleaned and formatted for this app. Data includes covid cases and deaths by state since february 2020."),
                       p("Additional data for this app was used to further help visualize and understand the impact of COVID-19 on the United States by a variety of factors. Two datasets from the Center for Disease Control (CDC) were used. The first containing COVID deaths by age and gender (1) and the second containing COVID deaths by pre-existing condition and age (2). More information about these datasets can be found at the following links: (1)https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-S/9bhg-hcku 
(2)https://data.cdc.gov/NCHS/Conditions-contributing-to-deaths-involving-corona/hk9y-quqm
"), 
                       p("Another data source used comes from Ballotpedia. This source gives information on the political party of each of the state governors, including the governors of 5 U.S. territories. More information about this dataset can be found at the following link: https://ballotpedia.org/Partisan_composition_of_governors 
"),
                       p("The final data set used for this app is from the U.S. Census. This source contains information regarding the population of the United States broken down by state including information for the District of Columbia and Puerto Rico. More information about this dataset can be found at the following link: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/nst-est2019-alldata.pdf
"),
                       h2("HOW TO USE THE APP"),
                       p("This app will allow you to investigate COVID-19 in the United States with data collected since February 2020. This app will investigate its impact through four key ways. 
"),
                       p("Data Exploration:  First you will be able to explore COVID-19 in the United States by looking at the overall distribution of deaths in the country by a selection of variables; Age, Pre-existing Conditions, COVID-19 Testing Status and an overall COVID-19 deaths visualization. Additionally, you will be able to visualize these groups at an individual state level, and have the opportunity to look at specific age groups, gender and the change over time of COVID-19 deaths, positive cases and negative cases in each of the 50 states and Washington, DC and Puerto Rico.  
"),
                       p("Data Visualization: Next you will be able to visualize and compare confirmed cases and deaths and extreme progression of COVID-19 symptoms (use of ventilator) by age, by gender, by pre-existing conditions, by month, and by state in the United States. These will be visualized in two ways. First, you will be able to look at boxplots comparing the distribution of COVID-19 deaths for each group in the variable. In addition, you will also be able to look at bar graphs to look at which group has the most COVID-19 cases. You will also be able to visually investigate the relationship of age, existing conditions, month, and location (by state) to total COVID positive test results, extreme progression of the virus (individuals on ventilators), and COVID deaths. You will be able to see these relations through boxplots to get a sense of the distribution, bar graphs to get a sense of the total numbers, and a written summary output generated for each visualization. For each of the boxplots created you will have the choice as to whether to include outliers in the graph or not. If you choose to not include outliers, you can change the maximum value of the x-axis to make the graph easier to read. In addition, there are boxplots presented for the overall distribution of COVID-19 deaths, positive test results, and extreme progression of the virus (individuals on ventilators). 
"),
                       p("Mapping: You will be able to look at COVID-19 related facts on a map. In this map, you can choose whether to visualize COVID-19 deaths, number of positive test results, or number of negative test results. Once you select which of those three you would like to explore more, a map of the United States will appear, where each state is a different shade of red. The darker the red the state is, the more of that input there is. For example, if you choose \"death\" as what you would like to explore, the darker the red a state is the more deaths there are in that state compared to the other states. It is important to note that these are totals for the entire pandemic, not for specific months. In addition, if you were to click on any state on the map, the name of the state, as well as the number for that state would be presented. On the map, you can choose to zoom in or out, or move around more to focus on specific regions/states as needed. Based on the input you choose for the mapping, different t-tests will be run. They will compare whether the political party of state’s governors affect the number of COVID-19 deaths, positive test results, or negative test results. Whichever you choose to visualize on the map of the United States will be the same variable used in this t-test. The app presents the p-value, estimate, and 95% confidence interval from the t-test.
"), 
                       p("Statistical Analysis: Additional to the visualizations described above, you will have the opportunity to conduct several statistical analyses on the data previously described. Specifically, you will be able to conduct both a one-way and two-way analysis of variance (ANOVA) to investigate if there is a statistically significant difference in COVID-19 outcomes. For analysis in this app, all ANOVA assumptions are assumed to be met for the data. To learn more about ANOVA’s please follow this link: https://www.sciencedirect.com/topics/nursing-and-health-professions/analysis-of-variance 
"),
                       p("You will be able to conduct an one-way or two-way ANOVA on two populations. You will have the opportunity to look at the general population and conduct an ANOVA on COVID-19 deaths by age, location, gender or a combination of these variables if you choose a two-way ANOVA. Additionally, you will have the opportunity to look at the individuals with a pre-existing condition and conduct an ANOVA on COVID-19 deaths by age, condition, condition type and location or a combination of these variables if you choose a two-way ANOVA. Results of an ANOVA analysis will tell you if there is a statistically significant difference between groups in relation to your outcome choice. You will be able to determine this through looking at the p-values for each group, which will be presented to you and determining if it is below 0.05. If the p-value is below 0.05, you can determine there is a significant difference between the groups you are investigating, if the value is above 0.05 you cannot determine there is a significant difference between the groups you are investigating. Visualization for this analysis will also be created through box-plots.
")),
              tabPanel ("Data Exploration: Overall and by State",
                        fluidRow(
                          
                          tabsetPanel(
                            tabPanel("Overall U.S. situation",
                                     radioButtons("status",
                                                  "Please select a variable to explore?",
                                                  choices=dimension),
                                     plotOutput("plot4", width="100%", height=500)
                            ),
                            
                            tabPanel("The situation in each state", 
                                     selectInput("NAME","Which state do you want to explore?",choices = unique(as.factor(combine_dd$NAME))),
                                     selectInput("Age_Group","Which age group do you want to explore?",choices = unique(as.factor(combine_dd$Age_Group)), selected = "85+"),
                                     selectInput("Sex","Which Sex do you want to explore?",choices = unique(as.factor(combine_dd$Sex))),
                                     plotOutput("plot1", width="90%",height=250), 
                                     plotOutput("plot3", width="90%",height=250), 
                                     plotOutput("plot2",height=250)
                                     
                            )))
              ),
              tabPanel("Data Visualization",
                       fluidRow(title = "Inputs",
                                column(4,
                                       varSelectInput("boxvar1", "Please Select Two Variables to Graph", data = bplot2,
                                                      selected = "Month"),
                                       varSelectInput("boxvar2", label = NULL, data = bplot2,
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
                                 tableOutput("anova_1")))),
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
                                 tableOutput("anova_2")))),
              tabPanel("Maps Graphics",
                       sidebarLayout(
                         sidebarPanel(
                           varSelectInput("mapvar", "Please Select a Variable to Map", data = ctracking4,
                                          selected = "death"),
                           checkboxInput("relative", "map unit per 100,000", value = FALSE),
                           textOutput("intro"),
                           tableOutput("map_anova")
                         ), 
                         mainPanel(
                           leafletOutput("map", height = 500),
                           textOutput("warning2")
                         ) 
                       )
              )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$anova_1 <- renderTable({     ##changes for output 
    if (!!input$type_1 == "One-way"){
      aout1 <- aov(Deaths ~ age_gender[[input$groups_1.1]], data = age_gender) %>%
        tidy() %>%
        select(`Term` = term, 
               `Degrees of Freedom` = df, 
               `Sum of Squares` = sumsq, 
               `F Value` = statistic, 
               `P value` = p.value) 
      aout1$Term[1] <-  as.character(input$groups_1.1)
      print(aout1)
    }
    else if (!!input$type_1 == "Two-way"){   ##changes for output 
      aout1 <- aov(Deaths ~ age_gender[[input$groups_1.1]] + age_gender[[input$groups_1.2]], data = age_gender)%>%
        tidy() %>%
        select(`Term` = term, 
               `Degrees of Freedom` = df, 
               `Sum of Squares` = sumsq, 
               `F Value` = statistic, 
               `P value` = p.value)
      aout1$Term[1] <-  as.character(input$groups_1.1)
      aout1$Term[2] <-  as.character(input$groups_1.2)
      print(aout1)
    }})
  
  output$anova_2 <- renderTable({        ##changes for output
    if (!!input$type_2 == "One-way"){
 aout2 <-aov(Deaths ~ COVID_Deaths[[input$groups_2.1]], data = COVID_Deaths)%>%
        tidy() %>%
        select(`Term` = term, 
               `Degrees of Freedom` = df, 
               `Sum of Squares` = sumsq, 
               `F Value` = statistic, 
               `P value` = p.value)
 aout2$Term[1] <-  as.character(input$groups_2.1)
 print(aout2)
    }
    else if (!!input$type_2 == "Two-way"){      ##changes for output
aout2 <-aov(Deaths ~ COVID_Deaths[[input$groups_2.1]] + COVID_Deaths[[input$groups_2.2]], data = COVID_Deaths)%>%
        tidy() %>%
        select(`Term` = term, 
               `Degrees of Freedom` = df, 
               `Sum of Squares` = sumsq, 
               `F Value` = statistic, 
               `P value` = p.value)
aout2$Term[1] <-  as.character(input$groups_2.1)
aout2$Term[2] <-  as.character(input$groups_2.2)
print(aout2)
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
  
  # Proportions
  combine_1 <- reactive({ 
    combine_dd %>% 
      select(NAME,Age_Group,deaths,Condition_Group) %>% 
      filter(NAME==!!input$NAME,Age_Group==!!input$Age_Group) %>% 
      na.omit(deaths) %>% 
      unique() %>% 
      group_by(NAME,Age_Group,Condition_Group) %>% 
      summarize(deaths=sum(deaths)) %>% 
      mutate(Condition_Group = fct_reorder(as.factor(Condition_Group),deaths)) 
  })
  
  output$plot1 <- renderPlot({
    ggplot(combine_1(), aes(x=Condition_Group, y=deaths))+
      geom_col()+
      theme_bw()+
      coord_flip() +
      labs(x="Condition Group",
           y="Death toll",
           title="The overall number of deaths in different state and age group by condition group")
    
  })
  
  combine_2 <- reactive({ 
    ctracking_dd %>% 
      select(Month,state, deathIncrease,positiveIncrease,negativeIncrease) %>%
      mutate(NAME = state.name[match(state,state.abb)])%>%
      mutate(NAME = ifelse(state == "PR", "Puerto Rico",
                           ifelse(state == "DC", "District of Columbia",
                                  NAME))) %>%
      rename(death = deathIncrease,
             positive = positiveIncrease,
             negative = negativeIncrease)  %>%
      select(Month, NAME, state, everything()) %>%
      pivot_longer(cols=death:negative,
                   names_to = "tendence",
                   values_to="values") %>% 
      filter(NAME==!!input$NAME) %>%  
      unique() 
  })
  
  output$plot2 <- renderPlot({
    ggplot(combine_2(),aes(x=as.numeric(as.factor(Month)),y=values,colour= tendence))+
      geom_point(show.legend = F)+
      geom_line(show.legend = F)+
      labs(x="Month",
           y="Death toll",
           title="The number change of different tendence through month")+
      facet_wrap(~tendence,scales = "free")
    
  })
  
  combine_3 <- reactive({ 
    age_dd %>% 
      select(NAME,Sex,`Age group`,Deaths) %>% 
      filter(NAME==!!input$NAME,Sex==!!input$Sex) %>%
      filter(`Age group` == "18-29 years" | `Age group` == "30-49 years" |
               `Age group` == "50-64 years" | `Age group` == "65-74 years" |
               `Age group` == "75-84 years" | `Age group` == "85 years and over")
  })
  
  output$plot3 <- renderPlot({
    ggplot( combine_3(),aes(x=`Age group`,y=Deaths))+
      geom_col()+
      theme_bw()+
      coord_flip() +
      labs(x="Age Group",
           y="Death toll",
           title="The overall number of deaths in different states and gender by age group")
    
  })
  
  
  
  output$plot4  <- renderPlot({
    if(!!input$status == "Age_Group" ){
      age_dd %>%
        select(`Age group`,Deaths) %>% 
        group_by(`Age group`) %>% 
        summarise(Agedeath=sum(Deaths,na.rm = T)) %>% 
        ggplot(aes(x=`Age group`, y=Agedeath))+
        geom_col()+
        theme_bw()+
        coord_flip() +
        labs(x="Age Group",
             y="Death toll",
             title="The overall number of deaths in the United States by age group")  }
    
    else  if(!!input$status == "Condition"){
      death_dd %>% 
        select(Condition_Group,deaths) %>% 
        group_by(Condition_Group) %>% 
        summarise(Conditiondeath=sum(deaths,na.rm = T)) %>%
        ungroup() %>%
        mutate(Condition_Group = fct_reorder(as.factor(Condition_Group),Conditiondeath)) %>% 
        ggplot(aes(x=Condition_Group, y=Conditiondeath))+
        geom_col()+
        theme_bw()+
        coord_flip() +
        labs(x="Conditions",
             y="Death toll",
             title="The overall number of deaths in U.S by different condition") }
    else  if(!!input$status == "Status"){
      ctracking_dd %>% 
        dplyr::select(Month,deathIncrease,positiveIncrease,negativeIncrease) %>% 
        group_by(Month) %>% 
        unique() %>% 
        summarise(sumdeath=sum(deathIncrease,na.rm = T),
                  sumpositive=sum(positiveIncrease,na.rm = T),
                  sumnegative=sum(negativeIncrease,na.rm = T),
                  Month=unique(Month)) %>%
        pivot_longer(cols=sumdeath:sumnegative,
                     names_to = "Status",
                     values_to="values") %>% 
        ggplot(aes(x= as.numeric(as.factor(Month)), y= values, colour= Status))+
        geom_line(show.legend = F)+
        theme_bw()+
        facet_wrap(~Status,scales = "free")+
        labs(x="Month",
             y="Death toll",
             title="COVID-19 deaths, positive tests, and negative tests over time")}
    else  if(!!input$status == "COVID19"){
      ctracking_dd %>% 
        select(state,deathIncrease) %>% 
        group_by(state) %>%
        summarize(death = sum(deathIncrease, na.rm = T)) %>%
        ungroup() %>%
        ggplot(aes(x=reorder(state, death),y=death))+
        geom_col()+
        theme_bw()+ 
        coord_flip() +
        labs(x="State", 
             y="Death toll",
             title="The overall number of people who died of COVID19 in U.S by different state")
    }
    
  })
  
  
  #Mapping
  output$map <- renderLeaflet({
    validate(need(
      is.numeric(ctracking2[[input$mapvar]]) == "TRUE" & input$mapvar != "Month",
      "Please select a numeric variable"
    ))
    
    leaf <- leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(-98.483330, 38.712046, zoom = 4)
    
    if (input$relative == TRUE) {
      if (input$mapvar == "death") {
        # Color palette
        pal <- colorNumeric("Reds", domain=mapping$deaths_per_capita)
        
        # Setting up the pop up text
        popup_sb <- paste0(mapping$NAME,": ", mapping$deaths_per_capita)
        
        leaf %>%
          addPolygons(data = mapping, 
                      fillColor = ~pal(mapping$deaths_per_capita), 
                      fillOpacity = 0.7, 
                      weight = 0.2, 
                      smoothFactor = 0.2, 
                      popup = ~popup_sb) %>%
          addLegend(pal = pal, 
                    values = mapping$deaths_per_capita, 
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
        pal <- colorNumeric("Reds", domain=mapping$negative_test_per_capita)
        
        # Setting up the pop up text
        popup_sb <- paste0(mapping$NAME,": ", mapping$negative_test_per_capita)
        
        leaf %>%
          addPolygons(data = mapping , 
                      fillColor = ~pal(mapping$negative_test_per_capita), 
                      fillOpacity = 0.7, 
                      weight = 0.2, 
                      smoothFactor = 0.2, 
                      popup = ~popup_sb) %>%
          addLegend(pal = pal, 
                    values = mapping$negative_test_per_capita, 
                    position = "bottomright")
        
      } else if (input$mapvar == "Positive_Test") {
        
        # Color palette
        pal <- colorNumeric("Reds", domain=mapping$positive_test_per_capita)
        
        # Setting up the pop up text
        popup_sb <- paste0(mapping$NAME,": ", mapping$positive_test_per_capita)
        
        leaf %>%
          addPolygons(data = mapping , 
                      fillColor = ~pal(mapping$positive_test_per_capita), 
                      fillOpacity = 0.7, 
                      weight = 0.2, 
                      smoothFactor = 0.2, 
                      popup = ~popup_sb) %>%
          addLegend(pal = pal, 
                    values = mapping$positive_test_per_capita, 
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
    }
    else {
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
    }
    
  }) # End renderLeaflet
  
  output$map_anova <- renderTable({
    validate(need(
      is.numeric(ctracking2[[input$mapvar]]) == "TRUE" & input$mapvar != "Month",
      "Please select a numeric variable"
    ))
    
    if (input$relative == TRUE) {
      if (input$mapvar == "death") {
        t.test(ctracking2$deaths_per_capita ~ Party, data = ctracking2) %>%
          tidy() %>%
          select(`P-value` = p.value, estimate,
                 `95% Lower` = conf.low, `95% Upper` = conf.high)
      } else if (input$mapvar == "Positive_Test") {
        t.test(ctracking2$positive_test_per_capita ~ Party, data = ctracking2) %>%
          tidy() %>%
          select(`P-value` = p.value, estimate,
                 `95% Lower` = conf.low, `95% Upper` = conf.high)
      } else if (input$mapvar == "Negative_Test") {
        t.test(ctracking2$negative_test_per_capita ~ Party, data = ctracking2) %>%
          tidy() %>%
          select(`P-value` = p.value, estimate,
                 `95% Lower` = conf.low, `95% Upper` = conf.high)
      }
    }
    else {
      t.test(ctracking2[[input$mapvar]] ~ Party, data = ctracking2) %>%
        tidy() %>%
        select(`P-value` = p.value, estimate,
               `95% Lower` = conf.low, `95% Upper` = conf.high)
    }
    
  }) # End renderTable
  
  output$warning2 <- renderText({
    if (input$mapvar == "On_Ventilator") {
      "CAUTION: Incomplete data for number of COVID cases on Ventilator"
    } else if (input$mapvar == "Recovered") {
      "CAUTION: Incomplete data number of Recovered COVID Patients"
    }
  })
  
  output$intro <- renderText({
    "Testing to see if means differ by political party of Governor"
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
