
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Indicatori di Benchmarking regionale"),
  
  
  
  # Sidebar with a select input for indicator
  sidebarPanel(
    selectInput("key",
            "Nome indicatore:",
            c('Superficie' = 'demo_r_d3area',
              'Popolazione' = 'demo_r_d2jan'#,
              #'Tasso di attività' = 'edat_lfse_26'
              )
            )
  ),
  
  # Show a time series table of the indicator
  mainPanel(
      tableOutput("table"),
      plotOutput('plot')
  )
))
