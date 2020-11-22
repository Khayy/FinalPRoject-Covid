library(shiny)
library(tidyverse)
COVID_Deaths <- read_rds("../data/COVID_Deaths.rds")
ctracking <- read_rds("../data/ctracking.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Exploring Coronovirus Affects in the United States of America"),
  tabsetPanel(type = "tabs", 
              tabPanel("About",
                       h2("ABOUT"),
                       p("Explore the effects of coronavirus (COVID-19) at the individual state level with this shiny app. As we near the end of 2020 COVID-19 cases are on the rise again in the United States and understanding the effects by state can help you make informed decisions about your own wellbeing. This app will allow you to visualize and analyze COVID-19 cases and deaths across the United States. You can investigate coronavirus through exploring COVID positive results, progression to extreme medical attention (individuals on ventilators), and death rates through several other factors (age, location by state and pre-existing conditions). Additionally, you will be able to look at the general population statistics for the country and each state to better understand the impact COVID has had on the population. 
"), 
                       h2("DATA"),
                       p("The data on COVID cases and outcomes comes from the website covidtracking. More information about this data can be found at the following link: https://covidtracking.com/about-data. This data was cleaned and formatted for this app. Data includes covid cases and deaths by state since february 2020. "),
                       p("Additional data for this app was used to further help visualize and understand the impact of COVID-19 on the United States. Data on previously determined contributing factors to COVID-19 (pre-existing health conditions and causes) was used from the CDC. More information about this data set can be found at the following link: https://data.cdc.gov/NCHS/Conditions-contributing-to-deaths-involving-corona/hk9y-quqm 
"), 
                       h2("HOW TO USE THE APP"),
                       p("This app will allow you to investigate COVID-19 in the United States with data collected since February 2020. This app will investigate its impact through three key ways. 
"),
                       p("Data Exploration & Visualization: First you will be able to compare COVID-related deaths and COVID cases to the general population since February of 2020. This will allow you to see the proportion of people who have been diagnosed with COVID-19 in the United States. You will additionally be able to visualize confirmed cases and deaths due to COVID-19 by age, pre-existing conditions, and by state in the United States. These will be visualized through bar graphs. You will also be able to visually investigate the relationship of age, existing conditions, and location (by state) to total COVID positive test results, extreme progression of the virus (individuals on ventilators), and COVID deaths. You will be able to see these relations through boxplots and a written summary output generated for each visualization. 
"),
                       p("Statistical Analysis: Additional to the visualizations described above, you will have the opportunity to conduct several statistical analyses on the data previously described. Specifically, you will be able to conduct an analysis of variance (ANOVA) to investigate if there is a statistically significant difference in COVID outcomes. To learn more about ANOVA’s please follow this link: https://www.sciencedirect.com/topics/medicine-and-dentistry/analysis-of-variance 
"),
                       p("You will be able to conduct an one-way ANOVA on a selected outcome (COVID-19 positive case rate, progression to extreme medical attention rate and COVID-19 death rates) by age, location, and pre-existing condition diagnosis. Results of an one-way ANOVA analysis will tell you if there is a statistically significant difference between groups in relation to your outcome choice. For example an one-way ANOVA investigating location by COVID-19 death rates will tell you if there is a significant difference in death rates between different states. Visualization for this analysis will also be created through box-plots.
")), 
              tabPanel ("Data Exploration & Visualization"),
              tabPanel("Statistical Analysis")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)