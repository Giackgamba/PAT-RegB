
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
source('functions.R')

shinyServer(function(input, output) {
    
    
    output$table <- renderTable({
        id <- getSQLId(input$key)
        geoFilter <- 'UKE2+ITH2'
        filter <- getFilter(id)
        tab <- displayTab(input$key, paste0(filter, '.', geoFilter , '.'))
        tab
    })
    
    output$plot <- renderPlot({
        id <- getSQLId(input$key)
        geoFilter <- 'UKE2+ITH2'
        filter <- getFilter(id)
        tab <- getData(input$key, paste0(filter, '.', geoFilter , '.'))
        p <- ggplot(tab, aes(x = obsTime, y = obsValue, group = GEO, color = GEO))
        p+geom_line()+theme_bw()
        })
    
})
