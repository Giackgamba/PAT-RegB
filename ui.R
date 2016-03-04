
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
                            ),
                            "*: la variazione fa riferimento all'anno precedente"
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
                    height = 1500,
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
                    tabBox(
                        width = 12,
                        height = 950,
                        tabPanel(id = 'demo',
                                 width = 12,
                                 title = 'Demografia',
                                 comparisonOutput('fert', 4),
                                 comparisonOutput('mortinf', 5),
                                 comparisonOutput('asp', 8),
                                 comparisonOutput('incr', 22)
                        ),
                        tabPanel(id = 'salu',
                                 width = 12,
                                 title = 'Salute',
                                 comparisonOutput('tum', 10),
                                 comparisonOutput('inc', 11),
                                 comparisonOutput('cardio', 12)
                        ),
                        tabPanel(id = 'ist',
                                 width = 12,
                                 title = 'Istruzione',
                                 comparisonOutput('abb', 13),
                                 comparisonOutput('terz', 16)
                        ),
                        tabPanel(id = 'eco',
                                 width = 12,
                                 title = 'Economia',
                                 comparisonOutput('unloc', 17),
                                 comparisonOutput('redfam', 18),
                                 comparisonOutput('redlav', 20)
                        ),
                        tabPanel(id = 'lav',
                                 width = 12,
                                 title = 'Mercato del lavoro',
                                 comparisonOutput('att', 23),
                                 comparisonOutput('occ', 24),
                                 comparisonOutput('disoc', 25),
                                 comparisonOutput('disocgio', 28),
                                 comparisonOutput('partt', 29)
                        )
                    )
                )
            )
        )
    )
)