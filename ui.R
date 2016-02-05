
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
require(rCharts)
library(DT)
source('global.R')


sectors <- getSectors()

shinyUI(
    dashboardPage(
        
        # Application title
        dashboardHeader(title = "Benchmarking regionale",
                        titleWidth = 380
        ),
        
        
        
        # Sidebar with a select input for indicator
        dashboardSidebar(
            width = 380,
            sidebarMenu(
                id = 'sidebarmenu',
                menuItem('Dashboard',
                         tabName = 'dashboard',
                         icon = icon('dashboard')
                ),
                menuItem('Indicatori',
                         tabName = 'indicatori',
                         icon = icon('line-chart')
                ),
                conditionalPanel("input.sidebarmenu == 'indicatori'",
                                 selectInput(
                                     "settore",
                                     "Settore:",
                                     sectors, 
                                     selected = 1
                                 ),
                                 selectInput(
                                     'ind',
                                     'Indicatore:',
                                     c('optionA' = 'A')
                                 )
                ),
                #6 menuItem('Regioni',
                #          icon = icon('bank'),
                dataTableOutput('NUTS')
                #  )
            )
        ),
        
        dashboardBody(
#             tags$head(
#                 tags$link(rel = "stylesheet", type = "text/css", href = "agid.css")
#             ),
            tabItems(
                tabItem(
                    tabName = 'indicatori',
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
                                    #height = '100%',
                                    dataTableOutput('table')
                                ),
                                box(title = 'Grafico',
                                    width = 12,
                                    height = '50%',
                                    collapsible = F,
                                    showOutput('plot', 'highcharts')
                                )
#                                 valueBoxOutput('belowBox'),
#                                 valueBoxOutput('infoBox'),
#                                 valueBoxOutput('overBox'),
#                                 collapsible = T
                            ),
                            tabPanel('Testo',
                                     h1(
                                         textOutput('textTitle')
                                     ),
                                     textOutput('textTrento')
                            )
                        )
                    )
                ),
                
                tabItem(
                    'dashboard',
                    h1('DASHBOARD'),
                    fluidRow(
                        box(
                            'DOVE ANDIAMO BENE:',
                            background = 'green',
                            solidHeader = T,
                            renderText('textTitle')
                            
                            )
                    )
                )
            )
        )
    )
)

