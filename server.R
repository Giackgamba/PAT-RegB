
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(rCharts)
library(DT)
library(knitr)
library(dplyr)
source('functions.R')

options(
    DT.options = list(
        server = TRUE,
        searching = FALSE,
        pageLength = 12,
        paging = FALSE,
        processing = TRUE,
        escape = TRUE,
        rownames = F,
        language = list(
            info = ''
        )
    )
)

shinyServer(function(input, output, clientData, session) {
    
    sectors <- getSectors()
    geoFilter <- getGEOFilters()
    indicators <- reactive({ 
        getIndicators(input$settore) 
    })
    
    
    output$NUTS <- renderDataTable(datatable(
        getGEO(),
        escape = FALSE,
        rownames = FALSE
    )  %>%
        formatStyle(columns = 1:3, 
                    backgroundColor = '#222D32'
        )
    )
    
    observeEvent(
        input$settore, 
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
    
    observeEvent(
        input$ind,
        output$table <- DT::renderDataTable(
            ({
                data <- data()
                tab <- pivotData(data)
                tab <- datatable(tab,
                                 escape = FALSE,
                                 rownames = FALSE,
                                 selection = list(mode = 'multiple', 
                                                  selected = c(5,6)
                                 ),
                                 options = list(
                                     columnDefs = list(
                                         list(orderable = FALSE, 
                                              title = '',
                                              targets = 11)
                                     )
                                 )
                )
            })
        )
    )
    
    observeEvent(
        input$ind,
        output$plot <- renderChart2(
            ({
                data <- data()
                p <- makeInteractivePlot(data, input$table_rows_selected)
                p$addParams(dom = 'plot')
                
                return(p)
            })
        )
    )
    
    output$belowBox <- renderValueBox({
        valueBox(4, ' Regioni sotto PAT', 
                 color = 'green', 
                 icon = icon('thumbs-down'))
    })
    
    output$overBox <- renderValueBox({
        valueBox(7, ' Regioni sopra PAT', 
                 color = 'red', 
                 icon = icon('thumbs-up'))
    })
    
    output$infoBox <- renderValueBox({
        valueBox(7, ' Valore della PAT', 
                 color = 'yellow', 
                 icon = icon('newspaper-o'))
    })
    
    output$textTitle <- renderText({
        as.character(getIndName(input$ind))
    })
    
    output$textTrento <- renderText({
        data <- data()
        makeText(data)
    })
})
