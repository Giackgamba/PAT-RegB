
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
    
    observeEvent(input$ind,
                 output$table <- renderDataTable(
                     ({
                         data <- data()
                         tab <- pivotData(data)
                         tab
                     }), 
                     options = list(
                         searching = FALSE,
                         pageLength = 10
                     )
                 )
    )
    
    observeEvent(input$ind,
                 output$plot <- renderDygraph(
                     ({
                         
                         data <- data()
                         p <- makeInteractivePlot(data)
                         p
                     })
                 )
    )
    
    output$belowBox <- renderValueBox({
        valueBox(4, ' Regioni sotto PAT', color = 'green', icon = icon('thumbs-down'))
    })
    
    output$overBox <- renderValueBox({
        valueBox(7, ' Regioni sopra PAT', color = 'red', icon = icon('thumbs-up'))
    })
    
})
