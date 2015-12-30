
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
source('functions.R')


shinyServer(function(input, output, clientData, session) {
    
    sectors <- getSectors()
    geoFilter <- getGEOFilters()
    indicators <- reactive({ 
        getIndicators(input$settore) 
    })
    
    observeEvent(input$settore, 
                 ({ 
                     updateSelectInput(session, 'ind', choices = indicators()) 
                 })
    )
    
    id <- reactive({ 
        getSQLId(input$ind) 
    })
    
    conceptFilter <- reactive({
        getFilters(id()) 
    })
    
    filter <- reactive({ 
        paste0(conceptFilter(), '.', geoFilter, '.') 
    })
    
    data <- reactive({ 
        getData(input$ind, filter()) 
    })
    
    output$table <- renderDataTable(
        ({
            data <- data()
            tab <- pivotData(data)
            tab
        }), 
        options = list(
            paging = FALSE,
            searching = FALSE
            )
    )
    
    output$plot <- renderPlot({
        data <- data()
        p <- ggplot(data, aes(x = obsTime, y = obsValue, group = GEO, color = GEO))
        p+geom_line()+theme_minimal()
    })
    
    output$belowBox <- renderValueBox({
        valueBox(4, ' Regioni sotto PAT', color = 'green')
    })
    
    output$overBox <- renderValueBox({
        valueBox(7, ' Regioni sopra PAT', color = 'red')
    })
    
})
