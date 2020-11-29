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
              tabPanel("Data Exploration & Visualization"),
              tabPanel("Statistical Analysis: General Population"),
              tabPanel("Statistical Analysis: Individuals With A Pre-existing Condition")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)
