
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

comparison <- function(input, output, session, ind) {
    output$table <- renderTable({
        getComparison(ind) %>%
            select(Rank = rank, Geo = GEO, Valore = obsValue)
    })
}

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
                tab <- datatable(
                    tab,
                    selection = list(mode = "multiple", 
                                     selected = c(5,6)
                    ),
                    escape = FALSE,
                    options = list(
                        columnDefs = list(
                            list(orderable = FALSE, 
                                 title = "",
                                 targets = -1)
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
        
        output$textBest <- renderTable({
            res <- getBestTN() %>%
                select(Indicatore = ind,
                       Valore = obsValue,
                       Rank = rank) %>%
                arrange(Rank)
        },
        include.rownames = F)
        
        output$textWorst <- renderTable({
            res <- getWorstTN() %>%
                select(Indicatore = ind,
                       Valore = obsValue,
                       Rank = rank) %>%
                arrange(desc(Rank))
        },
        include.rownames = F)  
        
        output$area <- callModule(comparison, "area", "demo_r_d3area")
        output$popolazione <- callModule(comparison, "popolazione", "demo_r_d2jan")
        output$fertilita <- callModule(comparison, "fertilita", "demo_r_frate2")
        output$mortinf <- callModule(comparison, "mortinf", "demo_r_minfind")
        output$nati <- callModule(comparison, "nati", "demo_r_births")
        output$morti <- callModule(comparison, "morti", "demo_r_magec")
        output$spevita <- callModule(comparison, "spevita", "demo_r_mlifexp")
        
})
    