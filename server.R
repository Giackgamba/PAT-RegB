
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
require(rCharts)
source('functions.R')

options(
    DT.options = list(
        searching = FALSE,
        pageLength = 12,
        paging = F,
        processing = T,
        language = list(info = ''),
        columnDefs = list(
            list(orderable = FALSE,
                 targets = 12)
        )
    )
)

shinyServer(function(input, output, clientData, session) {
    
    sectors <- getSectors()
    geoFilter <- getGEOFilters()
    indicators <- reactive({ 
        getIndicators(input$settore) 
    })
    
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
        output$table <- renderDataTable(
            ({
                data <- data()
                tab <- pivotData(data)
                tab <- datatable(
                    mutate(
                        tab,
                        img = ifelse(
                            tab[, 11] > tab[, 10], 
                            '<img src="images/uparrow137.svg"></img>', 
                            '<img src="images/down103.svg"></img>'
                        )
                    ),
                    escape = FALSE
                )
            })
        )
    )
    
    observeEvent(
        input$ind,
        output$plot <- renderChart2(
            ({
                
                data <- data()
                p <- makeInteractivePlot(data)
                p$addParams(dom = 'plot')
                p$chart(zoomType = 'xy')
                p$xAxis(title = 'Anno', rotation = 45)
                p$yAxis(title = 'Valore', format = '{point.y:,.0f}')
                return(p)
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
