
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinydashboard)
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
            tags$head(
                tags$link(rel = "stylesheet", 
                          type = "text/css", 
                          href = "custom.css")
            ),
            tabItems(
                tabItem(
                    tabName = 'indicatori',
                    fluidRow(
                        tabBox(
                            width = 12,
                            height = 1250,
                            tabPanel(
                                'Tabella',
                                box(title = 'Serie Storica',
                                    collapsible = F,
                                    width = 12,
                                    #height = '100%',
                                    DT::dataTableOutput('table')
                                )
                            ),
                            tabPanel(
                                'Grafico',
                                     box(title = 'Grafico',
                                         width = 12,
                                         height = '50%',
                                         collapsible = F,
                                         showOutput('plot', 'highcharts')
                                     )
                            )
                        )
                    )
                ),
                
                tabItem(
                    'dashboard',
                    h1('Dashboard'),
                    fluidRow(
                        box(id = 'good',
                            width = 6,
                            title = 'Dove andiamo bene:',
                            solidHeader = T,
                            tableOutput('textBest')
                        ),
                        box(id = 'bad',
                            width = 6,
                            title = 'Dove andiamo male:',
                            solidHeader = T,
                            tableOutput('textWorst')
                        )
                    ),
                    fluidRow(
                        comparisonOutput(id = 'area', "Area"),
                        comparisonOutput('popolazione', "Popolazione"),
                        comparisonOutput('fertilita', "Fertilità"),
                        comparisonOutput('mortinf', "Mortalità infantile"),
                        comparisonOutput('nati', "Nascite"),
                        comparisonOutput('morti', "Decessi"),
                        comparisonOutput('spevita', "Speranza di vita a 65 anni")
                    )
                )
            )
        )
    )
)