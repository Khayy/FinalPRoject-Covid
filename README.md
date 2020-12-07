# Exploring Coronovirus Affects in the United States of America
This app explores the effects of coronavirus (COVID-19) at the individual state level with this shiny app. As we near the end of 2020 COVID-19 cases are on the rise again in the United States and understanding the effects by state can help you make informed decisions about your own wellbeing. This app will allow you to visualize and analyze COVID-19 cases and deaths across the United States. You can investigate coronavirus through exploring COVID positive results, progression to extreme medical attention (individuals on ventilators), and death rates through several other factors (age, sex, location by state and pre-existing conditions). Additionally, you will be able to look at the general population statistics for the country and each state to better understand the impact COVID has had on the population.

this app requires the following packages to run:
shiny (version 1.5.0) <br />
shinythemes (version 1.1.2)<br />
fresh (version 0.2.0)<br />
tidyverse (version 1.3.0)<br />
broom (version 0.7.0)<br />
sf (version 0.9-6)<br />
tigris (version 1.0)<br />
leaflet (version 2.0.3)<br />
viridis (version 0.5.1)<br />

For a more complete listing of how each package is used, please see the Required Packages section of the vignette.

For information regarding the data used in building this app, please see the Data Source and Structure in the vignette.

This app allows users to investigate COVID-19 in the United States with data collected since February 2020 in four key ways.

The first way is through graphics. The user will be able to visualize and compare confirmed cases and deaths and extreme progression of COVID-19 symptoms (use of ventilator) by age, by gender, by pre-existing conditions, by month, and by state in the United States. The user will be able to make bar graphs and boxplots by each of those conditions to compare number of COVID-10 deaths, cases, and number of people on a ventilator. The user will be able to coose which comparisons they make. There is also the option for the user to look at individual states.

Next, the user will be able to look at a map of the United States in order to compare deaths, positive tests, and negative tests by state (the user can choose which of these to map). The User will also be able to choose whether to lok at the raw numbers, or whether to look at the number of cases per 100,000 people in order to make comparision easier. In addition to the mapping, the user will also see a t-test to see if the politial party of the state's governor affects the cases. 

Next the user can run either a one-way or two-way ANOVA. The user can also choose whether to run the ANOVA for the population overall or specifically for those with pre-existing conditions. The user will have the opportunity to look at the individuals with a pre-existing condition and conduct an ANOVA on COVID-19 deaths by age, condition, condition type and location or a combination of these variables if you choose a two-way ANOVA. Results of an ANOVA analysis will tell you if there is a statistically significant difference between groups in relation to your outcome choice. 

For more information regarding the data vizualizations or statisitcal analyses run in this app, please reference the EDA Inputs, Controls, and Outputs and Statistical Analysis Inputs, Controls, and Outputs sections of the vignette.

