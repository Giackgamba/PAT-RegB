
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinydashboard)
require(rCharts)
source('functions.R')


sectors <- getSectors()

shinyUI(
    dashboardPage(
            
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
                ),
                dataTableOutput('NUTS')
            ),
            
            dashboardBody(
                
                fluidRow(
                    
                    tabBox(
                        width = 12,
                        height = 1250,
                        tabPanel(
                            'Overview',
                            box(title = 'Serie Storica',
                                color = 'green',
                                collapsible = F,
                                width = 12,
                                height = '100%',
                                dataTableOutput('table')
                            ),
                            box(title = 'Grafico',
                                width = 12,
                                height = '50%',
                                collapsible = F,
                                showOutput('plot', 'Highcharts')
                            ),
                            valueBoxOutput('belowBox'),
                            valueBoxOutput('infoBox'),
                            valueBoxOutput('overBox'),
                            collapsible = T
                        ),
                        tabPanel('Testo',
                                 h1(
                                     textOutput('textTitle')
                                 ),
                                 textOutput('textTrento')
                                 )
                    )
                )
            )
        )
)
