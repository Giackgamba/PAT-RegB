
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
source('functions.R')

shinyServer(function(input, output) {
   
  output$table <- renderDataTable({
    
    tab <- getData(input$key, '..TOTAL.ITH4.')
    tab
    
  })
  
})
