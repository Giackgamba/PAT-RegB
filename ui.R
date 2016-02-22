
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
        title = "Benchmarking regionale",
        
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
                dataTableOutput('NUTS')
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
                    h1(textOutput('title')),
                    tabBox(
                        width = 12,
                        height = 650,
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
                ),
                tabItem(
                    tabName = 'dashboard',
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
                    
                    # fluidRow(htmlOutput("boxes")),
                    fluidRow(
                        box(id = 'demo',
                            width = 12,
                            title = 'Demografia',
                            collapsible = T,
                            collapsed = T,
                            comparisonOutput('fert', 4),
                            comparisonOutput('mortinf', 5),
                            comparisonOutput('asp', 8),
                            comparisonOutput('incr', 22)
                        ),
                        box(id = 'salu',
                            width = 12,
                            title = 'Salute',
                            collapsible = T,
                            collapsed = T,
                            comparisonOutput('tum', 10),
                            comparisonOutput('inc', 11),
                            comparisonOutput('cardio', 12)
                        ),
                        box(id = 'ist',
                            width = 12,
                            title = 'Istruzione',
                            collapsible = T,
                            collapsed = T,
                            comparisonOutput('abb', 13),
                            comparisonOutput('terz', 16)
                        ),
                        box(id = 'eco',
                            width = 12,
                            title = 'Economia',
                            collapsible = T,
                            collapsed = T,
                            comparisonOutput('unloc', 17),
                            comparisonOutput('redfam', 18),
                            comparisonOutput('redlav', 20)
                        )
                    )
                )
            )
        )
    )
)