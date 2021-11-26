#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)

# Plot Wrappers 
plot_states <- function(covid_dataset){
    covid_dataset %>% 
        ggplot(aes(x=Date,y=x)) + geom_line(aes(color=State),size=1.2)+
        scale_x_date(limit=c(as.Date("2020-04-01"),as.Date("2021-12-30"))) +
        scale_y_continuous(labels=scales :: number_format(accuracy=1))+
        labs(title='Time Series for Confirmed Cases',subtitle = 'For top 5 worst affected states')+
        xlab(label='Time Period') +
        ylab(label='Confirmed Cases')
}

plot_districts <- function(covid_dataset){
    covid_dataset %>% 
        ggplot(aes(x=Date,y=x)) + geom_line(aes(color=District),size=1.2)+
        scale_x_date(limit=c(as.Date("2020-04-01"),as.Date("2021-12-30"))) +
        scale_y_continuous(labels=scales :: number_format(accuracy=1))+
        labs(title='Time Series for Confirmed Cases',subtitle = 'For top 5 worst affected states')+
        xlab(label='Time Period') +
        ylab(label='Confirmed Cases')
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("COVID-19 INDIA DASHBOARD"),

    fluidRow(
        column(6, offset = 3, wellPanel(
            h2("Global Statistics"),
            fluidRow(
                column(6, 
                       h5("Total Number of Confirmed Cases"),
                       h4(textOutput("gTotalConfirmed"))),
                column(6, 
                       h5("Total Number of Recovered Cases"),
                       h4(textOutput("gTotalRecovered"))),
            ),
            fluidRow(
                column(6, 
                       h5("Total Number of Testing"),
                       h4(textOutput("gTotalTested"))),
                column(6, 
                       h5("Total Number of Deceased Cases"),
                       h4(textOutput("gTotalDeceased"))),
            ),
            fluidRow(
                column(6, 
                       h5("Recovery Ratio"),
                       h4(textOutput("gTotalRecoveryRatio"))),
                column(6, 
                       h5("Mortality Rate"),
                       h4(textOutput("gTotalMortalityRate"))),
            ),
        ))
    ),
    
    fluidRow(
        column(4, 
               offset=3, 
               selectInput("gOption", "Select Option", c("Confirmed", "Recovered", "Deceased" ))),
        column(8,offset=2, plotOutput("gPlot"))
    ),
    
    fluidRow(
        column(8, h2("Top 5 worst affected states"))
    ),
    
    
    fluidRow(
        column(4, 
               selectInput("optionState", "Select Option", c("Confirmed", "Recovered", "Deceased" ))),
    ),
    
    fluidRow(
        plotOutput("worstStates")
    ),
    
    # Affected districts in State
    
    fluidRow(
        column(8, h2("Districts Analysis"))
    ),
    
    
    fluidRow(
        column(4, 
               selectInput("optionDistrictStateSelection", "Select State", c())),
        
        column(4, 
               selectInput("optionDistrictCaseType", "Select Cases Type", c("Confirmed", "Recovered", "Deceased"))),
    ),
    
    fluidRow(
        plotOutput("worstDistricts")
    ),
    
    
    # District Analysis
    fluidRow(
        column(8, h2("Analyse Districts"))
    ),
    
    
    fluidRow(
        column(4, 
               selectInput("optionDistrictStates", "Select State", c())),
        
        column(4, 
               selectInput("optionStateDistricts", "Select District", c())),
    ),
    
    fluidRow(
        plotOutput("districtAnalysis")
    ),
    
    # Vaccine 
    
    fluidRow(
        column(8, h2("Analyse Vaccination Progress"))
    ),
    
    fluidRow(
        column(4, 
               selectInput("optionVaccineState", "Select State", c())),
    ),
    
    fluidRow(
        plotOutput("vaccinationPlot")
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    dcovid <- read.csv("in_cov_district.csv")
    vaccine <- read.csv("covid_vaccine_statewise.csv")
    dcovid$Date <- as.Date(dcovid$Date)
    vaccine$Updated.On <- dmy(vaccine$Updated.On)
    date_df <- aggregate(dcovid$Confirmed, by = list(Date=dcovid$Date), FUN=sum)
    
    output$gTotalRecovered <- renderText({
        sum(dcovid$Recovered)
    })
    
    output$gTotalConfirmed <- renderText({
        sum(dcovid$Confirmed)
    })
    
    output$gTotalTested <- renderText({
        dcovid[is.na(dcovid)] = 0
        sum(dcovid$Tested)
    })
    
    output$gTotalDeceased <- renderText({
        sum(dcovid$Deceased)
    })
    
    output$gTotalMortalityRate <- renderText({
        (sum(dcovid$Deceased)/sum(dcovid$Confirmed)) * 100
    })
    
    output$gTotalRecoveryRatio <- renderText({
        (sum(dcovid$Recovered)/sum(dcovid$Confirmed)) * 100
    })
    
    output$gPlot <- renderPlot({
        if(input$gOption == "Confirmed"){
            date_df <- aggregate(dcovid$Confirmed, by=list(Date=dcovid$Date), FUN=sum)
        }
        if(input$gOption == "Recovered"){
            date_df <- aggregate(dcovid$Recovered, by=list(Date=dcovid$Date), FUN=sum)
        }
        if(input$gOption == "Deceased"){
            date_df <- aggregate(dcovid$Deceased, by=list(Date=dcovid$Date), FUN=sum)
        }
        
        ggplot(data=date_df, aes(x=Date, y=x)) + geom_line() + 
            labs(title=paste("Time Series for ", input$gOption, "cases"), subtitle = '') + 
            scale_y_continuous(labels=scales :: number_format(accuracy=1)) + 
            xlab(label='Time Period') + 
            ylab(label= paste(input$gOption,' Cases'))
    })
    
    output$worstStates <- renderPlot({
        agg_states_df <- aggregate(dcovid[,input$optionState], by=list(State=dcovid$State), FUN=sum)
        top_5 <- agg_states_df %>% 
                arrange(desc(x)) %>%
                top_n(5)
        tw <- as.vector(top_5$State)
        filtered_data = filter(dcovid, dcovid$State %in% tw)
        agg_states_cases <- aggregate(filtered_data[,input$optionState], by=list(Date=filtered_data$Date, State=filtered_data$State), FUN=sum)
        print(str(agg_states_cases))
        plot_states(agg_states_cases)
    })
    
    updateSelectInput(session, "optionDistrictStates", choices=unique(dcovid$State))
    
    updateSelectInput(session, "optionDistrictStateSelection", choices=unique(dcovid$State))
    
    updateSelectInput(session, "optionVaccineState", choices=unique(dcovid$State))
    
    observeEvent(input$optionDistrictStates, updateSelectInput(session, "optionStateDistricts", 
                                                               choices = unique(filter(dcovid, dcovid$State %in% c(input$optionDistrictStates))$District) ))
    
    output$worstDistricts <- renderPlot({
        district_data <- filter(dcovid, dcovid$State %in% c(input$optionDistrictStateSelection))

        agg_states_df <- aggregate(district_data[,input$optionDistrictCaseType], by=list(District=district_data$District), FUN=sum)

        top_5 <- agg_states_df %>% 
            arrange(desc(x)) %>%
            top_n(5)
        twd <- as.vector(top_5$District)
        filtered_data = filter(district_data, district_data$District %in% twd)
        agg_states_cases <- aggregate(filtered_data[,input$optionDistrictCaseType], by=list(Date=filtered_data$Date, District=filtered_data$District), FUN=sum)
        print(head(agg_states_cases, 4))
        plot_districts(agg_states_cases)
    })

    
    output$districtAnalysis <- renderPlot({
        if(!is.null(input$optionStateDistricts)){
            district_data <- filter(dcovid, dcovid$District %in% c(input$optionStateDistricts))
            ggplot(data = district_data, aes(x=Date)) + 
                geom_line(aes(y=Recovered, color="Recovered")) +
                geom_line(aes(y=Confirmed, color="Confirmed")) +
                geom_line(aes(y=Deceased, color="Deceased"))
        }
    })
    
    output$vaccinationPlot <- renderPlot({
        stateData <- filter(vaccine, vaccine$State %in% c(input$optionVaccineState))
        ggplot(data=stateData, aes(x=Updated.On)) + 
            geom_line(aes(y=Total.Doses.Administered, color="Total Doses")) +
            geom_line(aes(y=First.Dose.Administered, color="First Doses")) +
            geom_line(aes(y=Second.Dose.Administered, color="Second Doses")) +
            scale_y_continuous(labels=scales :: number_format(accuracy=1))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
