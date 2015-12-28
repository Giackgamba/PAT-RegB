
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Indicatori di Benchmarking regionale"),
  
  
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    selectInput("key",
            "Nome indicatore:",
            c('Superficie' = 'demo_r_d3area'
              ,'Popolazione' = 'demo_r_d2jan'
              )
            )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
      tableOutput("table")
  )
))
