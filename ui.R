
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinydashboard)
source('functions.R')


sectors <- getSectors()

shinyUI(dashboardPage(
    
    # Application title
    dashboardHeader(title = "Benchmarking regionale",
                    titleWidth = 300
    ),
    
    
    
    # Sidebar with a select input for indicator
    dashboardSidebar(
        width = 300,
        selectInput("settore",
                    "Settore:",
                    sectors, 
                    selected = 1
        ),
        
        selectInput('ind',
                    'Indicatore:',
                    c('optionA' = 'A')
        )
        
    ),
    
    # Show a time serie table of the indicator
    dashboardBody(
        div(
            fluidRow(
                box(title = 'Serie Storica',
                    color = 'green',
                    width = 12,
                    dataTableOutput("table")
                )
            )
        ),
        fluidRow(
            box(title = 'Grafico',
                solidHeader = TRUE,
                status = 'primary',
                width = 12,
                plotOutput('plot')
            )
        ),
        fluidRow(
            valueBoxOutput('belowBox'),
            valueBoxOutput('overBox')
        )
    )
))
