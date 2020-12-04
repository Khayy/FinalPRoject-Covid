library(shiny)
library(tidyverse)
library(broom)

 
# Load Data
death1 <- read_rds("../data/COVID_Deaths.rds") 
ctracking1 <- read_rds("../data/ctracking.rds")
age <- read_rds("../data/age_gender.rds") 
 
 
# tidy each table
death_1 <- death1 %>% 
   select(abb,NAME,population,`Condition Group`,`Age Group`,`Number of COVID-19 Deaths`) %>% 
   rename(deaths = "Number of COVID-19 Deaths",
          Age_Group = `Age Group`,
          state="abb",
          Condition_Group=`Condition Group`) %>%  
   mutate(Condition_Group=recode(Condition_Group,
                                 `All other conditions and causes (residual)`="other",
                                 `Intentional and unintentional injury, poisoning, and other adverse events`="adverse events"))

ctracking_1 <- ctracking %>% 
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


age_1 <- age %>% 
   rename(NAME="State")
age_1$Deaths[is.na(age_1$Deaths)]=0
 

 
# combine three data  
combine <- death_1 %>% 
   inner_join(ctracking_1, by = "state") %>% 
   inner_join(age_1, by = "NAME")
 

dimension <- c("Age_Group","Condition","Status","Sex")

ui <- fluidPage(
   radioButtons("status",
                "Do you want to know which dimension of death in the United States?",
                choices=dimension), 
   selectInput("NAME","Which state do you want to check?",choices = unique(as.factor(combine$NAME))),
   selectInput("Age_Group","Which age group do you want to check?",choices = unique(as.factor(combine$Age_Group)),,selected = "85+"),
   selectInput("Sex","Which Sex do you want to check?",choices = unique(as.factor(combine$Sex))),
   plotOutput("plot1"),
   plotOutput("plot2"),
   plotOutput("plot3"),
   plotOutput("plot4")

)

server <- function(input, output,session) {
   combine_1 <- reactive({ 
         combine %>% 
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
        labs(x="Age Group",
             y="Death Number",
             title="The overall number of deaths in the United States by age group")
      
  })
  
  combine_2 <- reactive({ 
     combine %>% 
        select(Month,NAME,death,deathIncrease,positive,positiveIncrease,negative,negativeIncrease) %>% 
        pivot_longer(cols=death:negativeIncrease,
                     names_to = "Status",
                     values_to="values") %>% 
        filter(NAME==!!input$NAME) %>%  
        unique()  
  })
  
  output$plot2 <- renderPlot({
     ggplot(combine_2(),aes(x=as.numeric(as.factor(Month)),y=values,colour=Status))+
        geom_point()+
        geom_line()+
      labs(x="Month",
           y="Death Number",
           title="The number change of different status through month")+
        facet_wrap(~Status,scales = "free")
     
  })
  
  combine_3 <- reactive({ 
     combine %>% 
        select(NAME,Sex,`Age group`,Deaths) %>% 
        filter(NAME==!!input$NAME,Sex==!!input$Sex) 
  })
  
  output$plot3 <- renderPlot({
     ggplot( combine_3(),aes(x=`Age group`,y=Deaths))+
        geom_col()+
        theme_bw()+
        coord_flip() +
        labs(x="Age Group",
             y="Death Number",
             title="The overall number of deaths in the United States by age group")
     
  })
  
  
  

  output$plot4  <- renderPlot({
     if(!!input$status == "Age_Group" ){
        combine %>%
           select(Age_Group,deaths) %>% 
           group_by(Age_Group) %>% 
           summarise(Agedeath=sum(deaths,na.rm = T)) %>% 
           filter(`Age_Group` != "All Ages" & `Age_Group` != "Not stated" ) %>% 
           ggplot(aes(x=Age_Group, y=Agedeath))+
           geom_col()+
           theme_bw()+
           coord_flip() +
           labs(x="Age Group",
                y="Death Number",
                title="The overall number of deaths in the United States by age group")  }
     
     else  if(!!input$status == "Condition"){
         combine %>% 
              select(Condition_Group,deaths) %>% 
              group_by(Condition_Group) %>% 
              summarise(Conditiondeath=sum(deaths,na.rm = T)) %>%
              mutate(Condition_Group = fct_reorder(as.factor(Condition_Group),Conditiondeath)) %>% 
              ggplot(aes(x=Condition_Group, y=Conditiondeath))+
              geom_col()+
              theme_bw()+
              coord_flip() +
              labs(x="Conditions",
                   y="Death Number",
                   title="The overall number of deaths in U.S by different condition") }
        else  if(!!input$status == "Status"){
               combine %>% 
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
                      y="Death Number",
                      title="The overall number of deaths in U.S by different condition")}
           else  if(!!input$status == "Sex"){
                combine %>% 
                    select(Sex,Deaths,`Age group`) %>% 
                    filter(`Age group`=="18-29 years"|`Age group`=="30-49 years"|
                              `Age group`=="50-64 years"|`Age group`=="65-74 years"|
                              `Age group`=="75-84 years"|`Age group`=="85 years and over") %>%  
                    group_by(Sex) %>% 
                    summarize(sumDeaths=sum(Deaths,na.rm = T)) %>% 
                    ggplot(aes(x=Sex,y=sumDeaths)) +
                    geom_col()+ 
                    theme_bw()+
                    labs(x="Sex",
                         y="Death Number",
                         title="The overall number of deaths in U.S by different Sex") 
              }
       
  })
}
   
 

shinyApp(ui = ui, server = server)