## library to read the sdmx's
library(rsdmx)

## libraries for data manipulation
library(dplyr)
library(tidyr)

## libraries for interactive plot, table and map
library(DT)
library(rCharts)
library(leaflet)
library(rgdal)




## Read system tables, always offline
tabIndicators <-
    read.csv2("backupData/tabIndicatori.csv", stringsAsFactors = F)
tabNUTS <- read.csv2("backupData/tabNUTS.csv", stringsAsFactors = F)
tabConcepts <-
    read.csv2("backupData/tabConcepts.csv", stringsAsFactors = F)
tabSettori <-
    read.csv2("backupData/tabSettori.csv", stringsAsFactors = F)

#Map data
NUTS2 <- readOGR("Shapefile/NUTS_RG_10M_2013.shp", 
                 layer = "NUTS_RG_10M_2013", 
                 verbose = F, 
                 stringsAsFactors = F)
NUTS2_ALL <- subset(NUTS2, NUTS2@data$STAT_LEVL_ == 2)
NUTS2_SP <- subset(NUTS2, NUTS2@data$NUTS_ID %in% tabNUTS$id)

## Get the ind key, from ind id
getKey <- function(id) {
    key <- filter(tabIndicators, idDataFlow == id) %>%
        select(nome) %>%
        as.character()
    return(key)
}


## Get the concepts (filters) relative to the key DataFlow from EUROSTAT
downloadConcepts <- function(id) {
    BaseUrl <-
        "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_"
    key <- getKey(id)
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    concepts <- as.data.frame(DSD@concepts,
                              conceptSchemeId = paste0("CS_DSD_", key))
    return(concepts)
}

## Get the code list relative to the concept from EUROSTAT
downloadCodeList <- function(id, concept) {
    BaseUrl <-
        "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_"
    key <- getKey(id)
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    codelist <- as.data.frame(DSD@codelists,
                              codelistId = paste0("CL_", concept))
    return(codelist)
}

## Download the data, given the filter
downloadData <- function(id) {
    BaseUrl <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data"
    key <- getKey(id)
    filter <- paste0(getFilters(id), ".", getGEOFilters(), ".")
    dataUrl <- paste(BaseUrl,
                     key,
                     filter,
                     sep = "/")
    tryCatch(
        {
            dataz <- as.data.frame(readSDMX(dataUrl))
            print("primo try")
            write.csv2(dataz, 
                       paste0("backupData/", 
                              key, "-", 
                              id, ".csv"), 
                       row.names = F)
        },
        warning = function(w) {
            print("warning")
            dataUrl <- sub('MT', 'MT00', dataUrl)
            dataz <- as.data.frame(readSDMX(dataUrl))
            write.csv2(dataz, 
                       paste0("backupData/", 
                              key, "-", 
                              id, ".csv"), 
                       row.names = F)
            
        }
    )
    
    #return(dataz)
}

## Read the indicator file
getDataOffline <- function(id) {
    key <- getKey(id)
    dataz <- read.csv2(
        paste0("backupData/", key, "-", id, ".csv"),
        stringsAsFactors = F,
        quote = "\""
    )
    # read.csv2 trasforma le colonne 'T' in booleane
    # quindi li ritrasformiamo in character
    # (non che quelle colonne siano utilizzate)
    dataz[,sapply(dataz,class) == "logical"] <-
        sapply(dataz[,sapply(dataz,class) == "logical"],
               function(i)
                   substr(as.character(i),1,1))
    
    return(dataz)
}

## Wrapper, read from offline or download if older than 10 days
getData <- function(id) {
    key <- getKey(id)
    file <- paste0("backupData/", key, "-", id, ".csv")
    if (file.exists(file)) {
        old <-
            difftime(Sys.time(), file.info(file)$mtime, units = "days") > 1000
        if (!old)
            dataz <- getDataOffline(id)
        else dataz <- downloadData(id)
    } else dataz <- downloadData(id)
    
    lastYear <- dataz %>% 
        filter(GEO == 'ITH2' & obsValue != 'NA') %>%
        summarise(year = max(obsTime)) %>%
        as.numeric()
    direction <- filter(tabIndicators, idDataFlow == id) %>%
        select(direction) %>%
        as.character()
    dataz <- dataz %>% 
        filter(obsTime <= lastYear & obsValue != 'NA') %>%
        select(obsTime, GEO, obsValue) %>%
        group_by(obsTime, GEO) %>%
        summarise(obsValue = sum(obsValue)) %>%
        ungroup()
    
    rank <- dataz %>%
        group_by(obsTime) %>% 
        mutate(obsValue = min_rank(obsValue)) %>%
        ungroup()
    return(list(value = dataz,
                rank = rank,
                lastYear,
                direction
                )
           )
}

## Transform the data to have vertical year and horizontal Geo
pivotData <- function(x) {
    
    dataz <- x[[1]]
    dir <- x[[4]]
    img <- "<img src=\"arrow_DIR.svg\" height=\"16\" width = \"16\"></img>"
    
    if (all(c("GEO", "obsTime", "obsValue") %in% names(dataz))) {
        res <- dataz %>%
            select(GEO, obsTime, obsValue) %>%
            spread(obsTime, obsValue) %>%
            .[, c(1, (ncol(.) - ifelse(ncol(.)>8, 8, ncol(.)-2)):ncol(.))] %>%
            mutate(
                img = 
                    ifelse(
                        .[, ncol(.)] > .[, ncol(.)-1],
                        ifelse(
                            dir == "+",
                            sub("DIR", "up_green", img),
                            sub("DIR", "up_red", img)
                        ),
                        ifelse(
                            dir == "+",
                            sub("DIR", "down_red", img),
                            sub("DIR", "down_green", img)
                        )
                    )
            ) %>%
            arrange(GEO)
        
        return(res)
    } else
        cat("c'è stato qualche errore")
}

rankData <- function(x) {
    dataz <- x[[2]]
    img <- "<img src=\"arrow_DIR.svg\" height=\"16\" width = \"16\"></img>"
    
    if (all(c("GEO", "obsTime", "obsValue") %in% names(dataz))) {
        res <- dataz %>%
            select(GEO, obsTime, obsValue) %>%
            spread(obsTime, obsValue) %>%
            .[, c(1, (ncol(.) - ifelse(ncol(.)>8, 8, ncol(.)-2)):ncol(.))] %>%
            mutate(
                img = ifelse(
                    .[, ncol(.)] == .[, ncol(.)-1],
                    sub("DIR", "equal", img),
                    ifelse(
                        .[, ncol(.)] > .[, ncol(.)-1],
                        sub("DIR", "down_red", img),
                        sub("DIR", "up_green", img)
                    )
                )
            ) %>%
            arrange(GEO)
        
        return(res)
    }
}

## Helper to obtain the string to insert (manually) in the DB
getConceptsForSQL <- function(id) {
    lev <- levels(downloadConcepts(id)$id)
    a <- head(lev,-6) %>%
        paste(collapse = ".")
    return(a)
}

## Retrive the id starting from the name
getId <- function(key) {
    id <- as.integer(filter(tabIndicators, nome == key) %>%
                         select(idDataFlow))
    return(id)
}

## Retrive the filter values
getFilters <- function(id) {
    concepts <- filter(tabConcepts, idDataFlow == id) %>%
        select(value) %>%
        unlist() %>%
        paste0(collapse = ".")
    return(concepts)
}

## Retive sector (fixed)
getSectors <- function() {
    sectors <- tabSettori
    options <- list()
    for (i in 1:nrow(sectors)) {
        options[[as.character(sectors$descriz[i])]] <- sectors$idSettore[i]
    }
    
    return(options)
}

## Read the NUTS list
getGEOFilters <- function() {
    filters <- select(tabNUTS, id) %>%
        unlist() %>%
        paste0(collapse = "+")
    return(filters)
}

## Read the NUTS for the table in sidebar
getGEO <- function() {
    filters <- tabNUTS %>%
        transmute(
            id,Stato = paste0(
                "<img src=\"",
                stato,
                ".png\" height=\"24\" width = \"24\" alt = \"",
                stato,
                "\"></img>"
            ),
            Regione = descriz
            
        )
    return(filters)
}

## Get indicators
getIndicators <- function(idSector = NULL) {
    indicators <- tabIndicators
    if (!is.null(idSector))
        indicators <- filter(indicators, idSettore == idSector)
    
    return(indicators)
}

## List of indicators per sector to populate the dropdown
getIndicatorsList <- function(idSector = NULL) {
    indicators <- getIndicators(idSector)
    options <- list()
    for (i in 1:nrow(indicators)) {
        options[[as.character(indicators$descriz[i])]] <- indicators$idDataFlow[i]
    }
    return(options)
}

##
getIndName <- function(key) {
    if (!is.na(as.numeric(key))) {
        indicator <- tabIndicators %>%
            filter(idDataFlow == key) %>%
            select(descriz) %>%
            as.character()
    } else {
        indicator <- tabIndicators %>%
            filter(nome == key) %>%
            select(descriz) %>%
            as.character()
    }
    return(indicator)
}

## Build the plot
valuePlot <- function(data, selected) {
    dataz <- select(data[[1]], GEO, obsValue, obsTime) %>%
        mutate(obsTime = as.numeric(obsTime))
    
    colors <- colorRampPalette(
        c(rgb(0.596,0.2,0.318),
          rgb(0.667,0.349,0.224),
          rgb(0.153,0.459,0.325),
          rgb(0.376,0.592,0.196)
        )
    )(12)
    p <- hPlot(
        y = "obsValue",
        x = "obsTime",
        data = dataz,
        type = "line",
        group = "GEO",
        radius = 0
    )
    p$chart(zoomType = "xy")
    p$xAxis(title = list(text = "Anno"),
            categories = dataz$obsTime,
            tickInterval = 2
            )
    p$yAxis(title = list(text = "Valore"))
    p$colors(colors)
    
    selected <- pivotData(data)[selected, 1]
    
    # Black Magic
    p$params$series = lapply(seq_along(p$params$series), function(i) {
        
        x = p$params$series[[i]]
        x$visible = x$name %in% selected$GEO
        return(x)
    })
    return(p)
}

rankPlot <- function(data, selected) {
    dataz <- select(data[[2]], GEO, obsValue, obsTime) %>%
        mutate(obsTime = as.numeric(obsTime))
    
    colors <- colorRampPalette(
        c(rgb(0.596,0.2,0.318),
          rgb(0.667,0.349,0.224),
          rgb(0.153,0.459,0.325),
          rgb(0.376,0.592,0.196)
        )
    )(12)
    p <- hPlot(
        y = "obsValue",
        x = "obsTime",
        data = dataz,
        type = "line",
        group = "GEO",
        radius = 0
    )
    p$chart(zoomType = "xy")
    p$xAxis(title = list(text = "Anno"),
            categories = dataz$obsTime,
            tickInterval = 2
            )
    p$yAxis(title = list(text = "Rank"),
            reversed = "true",
            min = 1,
            max = 12,
            startOnTick = "false",
            endOnTick = "false",
            tickInterval = 1,
            gridLineWidth = 0.5)
    p$colors(colors)
    
    selected <- pivotData(data)[selected, 1]
    
    # Black Magic
    p$params$series = lapply(seq_along(p$params$series), function(i) {
        
        x = p$params$series[[i]]
        x$visible = x$name %in% selected$GEO
        return(x)
    })
    return(p)
}

getWholeData <- function() {
    indicators <- getIndicators()
    
    df <- sapply(
        indicators$idDataFlow, FUN = function(x)
            x =  getData(x)[[1]]
    )
    names(df) <- indicators$nome
    df <-
        Reduce(function(...)
            merge(..., by = c("obsTime", "GEO"), all = T), df)
    colnames(df) <- c("obsTime", "GEO", indicators)
    return(df)
}

getRank <- function(id) {
    direction <- filter(tabIndicators, idDataFlow == id) %>%
        select(direction) %>%
        as.character()
    
    df <- getData(id)[[1]] %>%
        filter(obsTime == max(obsTime)) %>%
        mutate(dir = direction) %>%
        mutate(rank = as.integer(
            ifelse(dir == "-", 
                   rank(obsValue), 
                   rank(desc(obsValue))
            )
        ) 
        )
    df
}

getBestTN <- function() {
    indicators <- getIndicators()
    
    df <- do.call(rbind, 
                  apply(
                      indicators, 
                      MARGIN = 1,
                      FUN = function(x){
                          d <- getRank(as.integer(x['idDataFlow'])) %>%
                              mutate(ind = x['descriz']) %>%
                              filter(GEO == 'ITH2' & rank<=3) 
                      }
                  )
    )
    df
}

getWorstTN <- function() {
    indicators <- getIndicators()
    
    df <- do.call(rbind, 
                  apply(
                      indicators, 
                      MARGIN = 1,
                      FUN = function(x){
                          d <- getRank(as.integer(x['idDataFlow'])) %>%
                              mutate(ind = x['descriz']) %>%
                              filter(GEO == 'ITH2' & rank>=10)
                      }
                  )
    )
    df
}

getComparison <- function(id) {
    df <-  getRank(id) %>%
        mutate(ind = id) %>%
        filter(GEO == 'ITH2' | rank %in% c(1, 2, nrow(.)-1, nrow(.))) %>%
        inner_join(tabNUTS, by = c('GEO' = 'id')) %>%
        select(rank, descriz, obsValue) %>%
        arrange(rank)
    df
}

## Start Comparison Module
## Comparison UI
comparisonUi <- function(id, ind) {
    options(ns.sep = "_")
    ns <- NS(id)
    
    nome <- tabIndicators %>% 
        filter(idDataFlow == ind) %>%
        select(descriz) %>%
        as.character(.)
    
    box(id = id,
        title = nome,
        width = 6,
        solidHeader = T,
        textOutput(ns("year")),
        tableOutput(ns("table")),
        actionButton(ns("appr"), "approfondisci")
    )
}

## Comparison Server
comparison <- function(input, output, session, ind) {
    
#     observeEvent(input$appr, {
#         browser()
#         print("click detected")
#         updateSelectInput(session, input$ind, selected = ind)
#         updateTabItems(session, "sidebarmenu", "indicatori")
#         print(ind)
#         print(input$ind)
#     })  
    
    output$year <- renderText({
        lastYear <- getData(ind)[[1]] %>% 
            filter(GEO == 'ITH2' & obsValue != 'NA') %>%
            summarise(year = max(obsTime)) %>%
            as.numeric()
        paste0("Anno di riferimento: ", lastYear)
    })
    
    output$table <- renderTable({
        getComparison(ind) %>%
            select(Rank = rank, Geo = descriz, Valore = obsValue)
    },
    include.rownames = F
    )
}
## End Comparison Module

### Mappe

makeMap <- function() {
    info <- getGEO()
    
    NUTS2_SP@data <- NUTS2_SP@data %>%
        full_join(info, by = c('NUTS_ID' = 'id'))
    
    centroids <- getSpPPolygonsLabptSlots(NUTS2_SP)
    
    leaflet() %>% 
        fitBounds( -2, 35, 12, 55) %>%
        addPolygons(data = NUTS2_ALL, 
                    weight = 1, 
                    color = "grey"
        ) %>%
        addPolygons(data = NUTS2_SP,
                    weight = 1,
                    color = "black",
                    fillColor = "#8B1F3F",
                    fillOpacity = 0.8,
                    popup = ~paste0("<b>", Regione, "</b>",
                                    "<br>",
                                    NUTS_ID, "\t", Stato)
        )
    
    
}

# comparisonUi <- function(id) {
# 
#     nome <- tabIndicators %>%
#         filter(idDataFlow == id) %>%
#         select(descriz) %>%
#         as.character(.)
# 
#     box(id = id,
#         title = nome,
#         width = 6,
#         solidHeader = T,
#         textOutput(paste0("year_", id)),
#         tableOutput(paste0("table_", id)),
#         actionButton(paste0("appr_",id), "approfondisci")
#     )
# }
