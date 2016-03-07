
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(rCharts)
library(DT)
library(dplyr)
source('global.R')

options(
    DT.options = list(
        server = FALSE,
        pageLength = 12,
        processing = TRUE,
        escape = FALSE,
        rownames = FALSE,
        language = list(
            info = ""
        ),
        dom = "t"
    )
)

shinyServer(function(input, output, clientData, session) {
    
    sectors <- getSectors()
    geoFilter <- getGEOFilters()
    indicators <- reactive({ 
        getIndicatorsList(input$settore) 
    })
    
    
    output$NUTS <- renderDataTable(
        datatable(
            getGEO(),
            escape = FALSE,
            rownames = FALSE,
            options = list(
                columnDefs = list(
                    list(width = '30%',
                         targets = c(0,1,2))
                )
            )
        )  %>%
            formatStyle(
                columns = 1:3, 
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
        getId(input$ind)
    })

    data <- reactive({
        input$ind
        getData(input$ind)
    })
    
    
    observeEvent(
        input$ind,
        output$table <- DT::renderDataTable(
            ({
                data <- data()
                tab <- pivotData(data)
                tab <- datatable(
                    tab,
                    selection = list(mode = "multiple", 
                                     selected = c(5,6)
                    ),
                    escape = FALSE,
                    options = list(
                        columnDefs = list(
                            list(orderable = FALSE, 
                                 title = "*",
                                 targets = -1)
                        )
                    )
                )
            })
        )
    )
    
    observeEvent(
        input$ind,
        output$title <- renderText(
            name <- getIndName(input$ind)
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
    
    output$textBest <- renderTable({
        res <- getBestTN() %>%
            select(Indicatore = ind,
                   Valore = obsValue,
                   Rank = rank) %>%
            arrange(Rank)
    },
    include.rownames = F,
    digits = c(0,0,1,0))
    
    output$textWorst <- renderTable({
        res <- getWorstTN() %>%
            select(Indicatore = ind,
                   Valore = obsValue,
                   Rank = rank) %>%
            arrange(desc(Rank))
    },
    include.rownames = F,
    digits = c(0,0,1,0))  
    
    
    output$fert <- callModule(comparison, 'fert', 4)
    output$mortinf <- callModule(comparison, 'mortinf', 5)
    output$asp <- callModule(comparison, 'asp', 8)
    output$incr <- callModule(comparison, 'incr', 22)
    output$fert <- callModule(comparison, 'tum', 10)
    output$mortinf <- callModule(comparison, 'inc', 11)
    output$asp <- callModule(comparison, 'cardio', 12)
    output$incr <- callModule(comparison, 'abb', 13)
    output$fert <- callModule(comparison, 'terz', 16)
    output$mortinf <- callModule(comparison, 'unloc', 17)
    output$asp <- callModule(comparison, 'redfam', 18)
    output$incr <- callModule(comparison, 'redlav', 20)
    output$incr <- callModule(comparison, 'att', 23)
    output$incr <- callModule(comparison, 'occ', 24)
    output$incr <- callModule(comparison, 'disoc', 25)
    output$incr <- callModule(comparison, 'disocgio', 28)
    output$incr <- callModule(comparison, 'partt', 29)
    
    output$map <- renderLeaflet(makeMap())
})
