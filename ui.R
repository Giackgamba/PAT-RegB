
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
    
    # Application title
    dashboardHeader(title = "Indicatori di Benchmarking regionale"),
    
    
    
    # Sidebar with a select input for indicator
    dashboardSidebar(
        selectInput("key",
                    "Nome indicatore:",
                    c('Superficie' = 'demo_r_d3area',
                      'Popolazione' = 'demo_r_d2jan'#,
                      #'Tasso di attività' = 'edat_lfse_26'
                    )
        )
    ),
    
    # Show a time series table of the indicator
    dashboardBody(
        fluidRow(
            box(tableOutput("table")),
            box(plotOutput('plot'))
        )
    )
))
