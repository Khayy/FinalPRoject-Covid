library(shiny)
library(tidyverse)
library(broom)

 
# Load Data
death <- read_rds("../data/COVID_Deaths.rds") 
ctracking <- read_rds("../data/ctracking.rds")
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
                                 `Intentional and unintentional injury, poisoning, and other adverse events`="adverse events"))

ctracking_dd <- ctracking %>% 
   select(Month,state,death,deathIncrease,negative,positive,
          negativeIncrease,positiveIncrease ) %>%
   filter(Month!=11) %>%
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


age_dd <- age %>% 
   rename(NAME="State")
age_dd$Deaths[is.na(age_dd$Deaths)]=0
 

 
# combine three data  
combine_dd <- death_dd %>% 
   inner_join(ctracking_dd, by = "state") %>% 
   inner_join(age_dd, by = "NAME")
 

dimension <- c("Age_Group","Condition","Status","COVID19")

<<<<<<< HEAD


=======
# UI start
>>>>>>> 09c0ddb7caf5dd0c3312256d5a803e9083897525
ui <- fluidPage(
  fluidRow(
    
   tabsetPanel(
     tabPanel("Overall U.S. situation",
              radioButtons("status",
                           "Do you want to know which dimension of death in the United States?",
                           choices=dimension),
              plotOutput("plot4", width="100%", height=500)
             ),
     
     tabPanel("The situation in each state", 
              selectInput("NAME","Which state do you want to check?",choices = unique(as.factor(combine_dd$NAME))),
              selectInput("Age_Group","Which age group do you want to check?",choices = unique(as.factor(combine_dd$Age_Group)), selected = "85+"),
              selectInput("Sex","Which Sex do you want to check?",choices = unique(as.factor(combine_dd$Sex))),
                     plotOutput("plot1", width="90%",height=250), 
                     plotOutput("plot3", width="90%",height=250), 
                     plotOutput("plot2",height=250)
                
              ))))
      
   

server <- function(input, output,session) {
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
     combine_dd %>% 
        select(Month,NAME,death,deathIncrease,positive,positiveIncrease,negative,negativeIncrease) %>% 
        pivot_longer(cols=death:negativeIncrease,
                     names_to = "tendence",
                     values_to="values") %>% 
        filter(NAME==!!input$NAME) %>%  
        unique()  
  })
  
  output$plot2 <- renderPlot({
     ggplot(combine_2(),aes(x=as.numeric(as.factor(Month)),y=values,colour= tendence))+
        geom_point()+
        geom_line()+
      labs(x="Month",
           y="Death toll",
           title="The number change of different tendence through month")+
        facet_wrap(~tendence,scales = "free")
     
  })
  
  combine_3 <- reactive({ 
     combine_dd %>% 
        select(NAME,Sex,`Age group`,Deaths) %>% 
        filter(NAME==!!input$NAME,Sex==!!input$Sex) 
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
        combine_dd %>%
           select(Age_Group,deaths) %>% 
           group_by(Age_Group) %>% 
           summarise(Agedeath=sum(deaths,na.rm = T)) %>% 
           filter(`Age_Group` != "All Ages" & `Age_Group` != "Not stated" ) %>% 
           ggplot(aes(x=Age_Group, y=Agedeath))+
           geom_col()+
           theme_bw()+
           coord_flip() +
           labs(x="Age Group",
                y="Death toll",
                title="The overall number of deaths in the United States by age group")  }
     
     else  if(!!input$status == "Condition"){
         combine_dd %>% 
              select(Condition_Group,deaths) %>% 
              group_by(Condition_Group) %>% 
              summarise(Conditiondeath=sum(deaths,na.rm = T)) %>%
              mutate(Condition_Group = fct_reorder(as.factor(Condition_Group),Conditiondeath)) %>% 
              ggplot(aes(x=Condition_Group, y=Conditiondeath))+
              geom_col()+
              theme_bw()+
              coord_flip() +
              labs(x="Conditions",
                   y="Death toll",
                   title="The overall number of deaths in U.S by different condition") }
        else  if(!!input$status == "Status"){
               combine_dd %>% 
                 dplyr::select(Month,death,deathIncrease,positive,positiveIncrease,negative,negativeIncrease) %>% 
                 group_by(Month) %>% 
                 unique() %>% 
                 summarise(sumdeath=sum(death,na.rm = T),
                           sumdeathIncrease=sum(deathIncrease,na.rm = T),
                           sumpositive=sum(positive,na.rm = T),
                           sumpositiveIncrease=sum(positiveIncrease,na.rm = T),
                           sumnegative=sum(negative,na.rm = T),
                           sumnegativeIncrease=sum(negativeIncrease,na.rm = T),
                           Month=unique(Month)) %>%
                 pivot_longer(cols=sumdeath:sumnegativeIncrease,
                              names_to = "Status",
                              values_to="values") %>% 
                 ggplot(aes(x= as.numeric(as.factor(Month)), y= values, colour= Status))+
                 geom_line()+
                 theme_bw()+
                 facet_wrap(~Status,scales = "free")+
                 labs(x="Conditions",
                      y="Death toll",
                      title="The overall number of deaths in U.S by different condition")}
           else  if(!!input$status == "COVID19"){
             combine_dd %>% 
               select(NAME,Condition_Group,Age_Group,deaths) %>% 
               filter(Condition_Group=="COVID-19"&Age_Group=="All Ages") %>% 
               unique() %>% 
               mutate(NAME=fct_reorder(as.factor(NAME),deaths)) %>% 
               ggplot(aes(x=NAME,y=deaths))+
               geom_col()+
               theme_bw()+ 
               coord_flip() +
               labs(x="State", 
                    y="Death toll",
                    title="The overall number of people who died of COVID19 in U.S by different state")
              }
       
  })
}
   
 

shinyApp(ui = ui, server = server)