library(tidyverse)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(readxl)
library(scales)
library(Hmisc)
library(BBmisc)
library(shiny)

tsl_odddata <- readxl:: read_excel("tsl_odddata.xlsx")
tsl_odddata <- tsl_odddata %>% mutate(Phome_bet365=(1/HomeOdd_bet365)*(1/(1/HomeOdd_bet365+1/AwayOdd_bet365+1/TieOdd_bet365))) %>%
    mutate(Paway_bet365=(1/AwayOdd_bet365)*(1/(1/HomeOdd_bet365+1/AwayOdd_bet365+1/TieOdd_bet365))) %>%
    mutate(Ptie_bet365=(1/TieOdd_bet365)*(1/(1/HomeOdd_bet365+1/AwayOdd_bet365+1/TieOdd_bet365))) %>% select(-HomeOdd_bet365,-AwayOdd_bet365,-TieOdd_bet365)
tsl_odddata <- tsl_odddata %>% mutate(Phome_betfair=(1/HomeOdd_betfair)*(1/(1/HomeOdd_betfair+1/AwayOdd_betfair+1/TieOdd_betfair))) %>%
    mutate(Paway_betfair=(1/AwayOdd_betfair)*(1/(1/HomeOdd_betfair+1/AwayOdd_betfair+1/TieOdd_betfair))) %>%
    mutate(Ptie_betfair=(1/TieOdd_betfair)*(1/(1/HomeOdd_betfair+1/AwayOdd_betfair+1/TieOdd_betfair))) %>% select(-HomeOdd_betfair,-AwayOdd_betfair,-TieOdd_betfair)
tsl_odddata <- tsl_odddata %>% mutate(Phome_betsson=(1/HomeOdd_betsson)*(1/(1/HomeOdd_betsson+1/AwayOdd_betsson+1/TieOdd_betsson))) %>%
    mutate(Paway_betsson=(1/AwayOdd_betsson)*(1/(1/HomeOdd_betsson+1/AwayOdd_betsson+1/TieOdd_betsson))) %>%
    mutate(Ptie_betsson=(1/TieOdd_betsson)*(1/(1/HomeOdd_betsson+1/AwayOdd_betsson+1/TieOdd_betsson))) %>% select(-HomeOdd_betsson,-AwayOdd_betsson,-TieOdd_betsson)
tsl_odddata <- tsl_odddata %>% mutate(Phome_bwin=(1/HomeOdd_bwin)*(1/(1/HomeOdd_bwin+1/AwayOdd_bwin+1/TieOdd_bwin))) %>%
    mutate(Paway_bwin=(1/AwayOdd_bwin)*(1/(1/HomeOdd_bwin+1/AwayOdd_bwin+1/TieOdd_bwin))) %>%
    mutate(Ptie_bwin=(1/TieOdd_bwin)*(1/(1/HomeOdd_bwin+1/AwayOdd_bwin+1/TieOdd_bwin))) %>% select(-HomeOdd_bwin,-AwayOdd_bwin,-TieOdd_bwin)
tsl_odddata <- tsl_odddata %>% mutate(Phome_pinnacle=(1/HomeOdd_pinnacle)*(1/(1/HomeOdd_pinnacle+1/AwayOdd_pinnacle+1/TieOdd_pinnacle))) %>%
    mutate(Paway_pinnacle=(1/AwayOdd_pinnacle)*(1/(1/HomeOdd_pinnacle+1/AwayOdd_pinnacle+1/TieOdd_pinnacle))) %>%
    mutate(Ptie_pinnacle=(1/TieOdd_pinnacle)*(1/(1/HomeOdd_pinnacle+1/AwayOdd_pinnacle+1/TieOdd_pinnacle))) %>% select(-HomeOdd_pinnacle,-AwayOdd_pinnacle,-TieOdd_pinnacle)

tsl_odddata <- tsl_odddata %>% select(-MatchId,-Home_score,-Away_score,-Weekday,-Round,-Hour,-Phome_betfair,-Paway_betfair,-Ptie_betfair) %>% filter(Season>2013)
tsl_train <- tsl_odddata %>% filter(Season<2017) %>% select(-Season)
tsl_model <- rpart(Result ~., data = tsl_train)

tsl_test <- tsl_odddata %>% filter(Season==2017) %>% select(-Season) %>% 
    filter(!grepl("goztepe|yeni-malatyaspor",Home)) %>%
    filter(!grepl("goztepe|yeni-malatyaspor",Away))
tsl_predict <- predict(tsl_model, newdata = tsl_test)

tsl_test <- tsl_test %>% rename(.,Home_team=Home,Away_team=Away)
tsl_predicted <- tsl_test %>% cbind(tsl_predict) %>% tbl_df
tsl_rev <- read_excel("tsl_odddata.xlsx") %>% filter(Season==2017)%>% 
    filter(!grepl("goztepe|yeni-malatyaspor",Home)) %>%
    filter(!grepl("goztepe|yeni-malatyaspor",Away)) %>% 
    mutate(Bhome_avg = (HomeOdd_bet365+HomeOdd_betsson+HomeOdd_bwin+HomeOdd_pinnacle)/4) %>%
    mutate(Baway_avg = (AwayOdd_bet365+AwayOdd_betsson+AwayOdd_bwin+AwayOdd_pinnacle)/4) %>%
    mutate(Btie_avg = (TieOdd_bet365+TieOdd_betsson+TieOdd_bwin+TieOdd_pinnacle)/4) %>%
    mutate(Round) %>%
    select(Bhome_avg,Btie_avg,Baway_avg,Round)
tsl_predicted <- tsl_predicted %>% cbind(tsl_rev) %>% select(Home_team,Away_team,Bhome_avg,Btie_avg,Baway_avg,Home,Tie,Away,Result,Round)
tsl_predicted <- tsl_predicted %>% rename(.,Phome=Home,Ptie=Tie,Paway=Away)
tsl_predicted <- tsl_predicted %>% mutate(Xhome = ((Phome * Bhome_avg)-1)/(Bhome_avg-1)) %>%
    mutate(Xtie = ((Ptie * Btie_avg)-1)/(Btie_avg-1)) %>%
    mutate(Xaway = ((Paway * Baway_avg)-1)/(Baway_avg-1))

tsl_predicted <- tsl_predicted %>% mutate(Advice = ifelse(Xhome>0&Xhome>Xaway&Xhome>Xtie,"Home",
                                                          ifelse(Xtie>0&Xtie>Xhome&Xtie>Xaway,"Tie",
                                                                 ifelse(Xaway>0&Xaway>Xhome&Xaway>Xtie,"Away","Do not bet"))))
tsl_predicted <- tsl_predicted %>% mutate(Phome_avg=(1/Bhome_avg)*(1/(1/Bhome_avg+1/Baway_avg+1/Btie_avg))) %>%
    mutate(Paway_avg=(1/Baway_avg)*(1/(1/Bhome_avg+1/Baway_avg+1/Btie_avg))) %>%
    mutate(Ptie_avg=(1/Btie_avg)*(1/(1/Bhome_avg+1/Baway_avg+1/Btie_avg))) %>% 
    select(Home_team,Away_team,Bhome_avg,Btie_avg,Baway_avg,Phome_avg,Ptie_avg,Paway_avg,Phome,Ptie,Paway,Xhome,Xtie,Xaway,Result,Advice,Round)

tsl_final <- tsl_predicted %>% mutate(Pdif = ifelse(Xhome>0&Xhome>Xaway&Xhome>Xtie,Phome-Phome_avg,
                                                    ifelse(Xtie>0&Xtie>Xhome&Xtie>Xaway,Ptie-Ptie_avg,
                                                           ifelse(Xaway>0&Xaway>Xhome&Xaway>Xtie,Paway-Paway_avg,0))))
tsl_final <- tsl_final %>% filter(Pdif > 0.07)

all_matchweeks <- sort(unique(tsl_final$Round))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Statistical Bet Modelling of Turkish Super League"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="Round",
                        label= "Matchweek",
                        choices= all_matchweeks ,
                        selected = "1",
                        multiple = TRUE,
                        selectize = TRUE)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           DT::dataTableOutput(outputId = "game_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$game_table <- DT::renderDataTable({
        tsl_shiny <- tsl_final %>%
            select(Round,Home_team,Away_team,Bhome_avg,Btie_avg,Baway_avg,Advice,Pdif) %>%
            filter(Round %in% input$Round) %>%
            mutate(Home_team=gsub("-"," ",Home_team),Away_team=gsub("-"," ",Away_team)) %>%
            mutate(Home_team=capitalizeStrings(Home_team,all.words=T),Away_team=capitalizeStrings(Away_team,all.words=T)) %>%
            mutate(Pdif=percent(Pdif),Bhome_avg=round(Bhome_avg,2),Btie_avg=round(Btie_avg,2),Baway_avg=round(Baway_avg,2)) %>%
            mutate(Game = paste(Home_team,sep = " - ",Away_team)) %>%
            mutate("Recommended Bet" = paste(Advice,sep = " + ",Pdif)) %>%
            select(Round,Game,Bhome_avg,Btie_avg,Baway_avg,"Recommended Bet") %>%
            rename(Week=Round,Home=Bhome_avg,Tie=Btie_avg,Away=Baway_avg) %>%
            arrange(Week)
        DT::datatable(data = tsl_shiny,
                      options = list(pageLength = 25),
                      rownames = F)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
