## library to read the sdmx's
library(rsdmx)

## libraries for data manipulation
library(dplyr)
library(tidyr)

## library to read form DB
library(RODBC)

## libraries for interactive plot and table
library(DT)
library(rCharts)


## Read system tables, always offline
tabIndicators <-
    read.csv2("backupData/tabIndicatori.csv", stringsAsFactors = F)
tabNUTS <- read.csv2("backupData/tabNUTS.csv", stringsAsFactors = F)
tabConcepts <-
    read.csv2("backupData/tabConcepts.csv", stringsAsFactors = F)
tabSettori <-
    read.csv2("backupData/tabSettori.csv", stringsAsFactors = F)

## Get the concepts (filters) relative to the key DataFlow from EUROSTAT
downloadConcepts <- function(key) {
    BaseUrl <-
        "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_"
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    concepts <- as.data.frame(DSD@concepts,
                              conceptSchemeId = paste0("CS_DSD_", key))
    return(concepts)
}

## Get the code list relative to the concept from EUROSTAT
downloadCodeList <- function(key, concept) {
    BaseUrl <-
        "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_"
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    codelist <- as.data.frame(DSD@codelists,
                              codelistId = paste0("CL_", concept))
    return(codelist)
}

## Download the data, given the filter
downloadData <- function(key, filter = NULL) {
    BaseUrl <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data"
    if (is.null(filter)) {
        nFilter <- nrow(downloadConcepts(key)) - 4
        filter <- paste(rep(".", nFilter - 1), collapse = "")
    }
    dataUrl <- paste(BaseUrl,
                     key,
                     filter,
                     sep = "/")
    data <- as.data.frame(readSDMX(dataUrl))
    write.csv2(data, paste0("backupData/", key, ".csv"), row.names = F)
    return(data)
}

## Read the indicator file
getDataOffline <- function(key) {
    data <- read.csv2(
        paste0("backupData/", key, ".csv"),
        stringsAsFactors = F,
        quote = "\""
    )
    # read.csv2 trasforma le colonne 'T' in booleane
    # quindi li ritrasformiamo in character
    # (non che quelle colonne siano utilizzate)
    data[,sapply(data,class) == "logical"] <-
        sapply(data[,sapply(data,class) == "logical"],
               function(i)
                   substr(as.character(i),1,1))
    return(data)
}

## Wrapper, read from offline or download if older than 10 days
getData <- function(key, filter = NULL) {
    file <- paste0("backupData/", key, ".csv")
    old <-
        difftime(Sys.time(), file.info(file)$mtime, units = "days") > 10
    if (file.exists(file) & !old)
        data <- getDataOffline(key)
    else
        data <- downloadData(key, filter)
    data <- select(data, obsTime, GEO, obsValue)
    return(data)
}

## Transform the data to have vertical year and horizontal Geo
pivotData <- function(x) {
    if (all(c("GEO", "obsTime", "obsValue") %in% names(x))) {
        res <- x %>%
            select(GEO, obsTime, obsValue) %>%
            spread(obsTime, obsValue) %>%
            .[, c(1, (ncol(.) - 8):ncol(.))] %>%
            mutate(
                img = ifelse(
                    .[, 10] > .[, 9],
                    "<img src=\"uparrow137.svg\" height=\"16\" width = \"16\"></img>",
                    "<img src=\"downarrow103.svg\" height=\"16\" width = \"16\"></img>"
                )
            ) %>%
            arrange(GEO)
        
        return(res)
    } else
        cat("c'è stato qualche errore")
}

## Helper to obtain the string to insert (manually) in the DB
getConceptsForSQL <- function(key) {
    lev <- levels(getConcepts(key)$id)
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
    concepts <- filter(tabIndicators, idDataFlow == id) %>%
        select(concepts)
    concepts <- as.character(concepts)
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
            id,
            Regione = descriz,
            Stato = paste0(
                "<img src=\"",
                stato,
                ".png\" height=\"24\" width = \"24\" alt = \"",
                stato,
                "\"></img>"
            )
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
        options[[as.character(indicators$descriz[i])]] <- indicators$nome[i]
    }
    return(options)
}

##
getIndName <- function(key) {
    indicator <- tabIndicators %>%
        filter(nome == key) %>%
        select(descriz)
    return(indicator)
}

## Build the plot
makeInteractivePlot <- function(data, selected) {
    data <- select(data, GEO, obsValue, obsTime) %>%
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
        data = data,
        type = "line",
        group = "GEO",
        color = "red",
        radius = 0
    )
    p$chart(zoomType = "xy")
    p$xAxis(categories = data$obsTime,
            title = list(text = "Anno"))
    p$yAxis(title = list(text = "Valore",
                         format = "{point.y:,.0f}"))
    p$colors(colors)
    selected <- pivotData(data)[selected, 1]
    # Black Magic
    p$params$series = lapply(seq_along(p$params$series), function(i) {
        x = p$params$series[[i]]
        x$visible = x$name %in% selected
        return(x)
    })
    return(p)
}


getWholeData <- function() {
    indicators <- getIndicators()$nome
    
    df <- lapply(
        indicators, FUN = function(x)
            x =  getData(x)
    )
    names(df) <- indicators
    df <-
        Reduce(function(...)
            merge(..., by = c("obsTime", "GEO"), all = T), df)
    colnames(df) <- c("obsTime", "GEO", indicators)
    return(df)
}

getRank <- function(x) {
    df <- getData(x) %>%
        filter(obsTime == max(obsTime)) %>%
        mutate(rank = as.integer(rank(desc(obsValue))))
}

getBestTN <- function() {
    indicators <- getIndicators()
    
    df <- do.call(rbind, 
                  apply(
                      indicators, 
                      MARGIN = 1,
                      FUN = function(x){
                          d <- getRank(x['nome']) %>%
                              mutate(ind = x['descriz']) %>%
                              filter(GEO == 'ITH2' & rank<=6) 
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
                          d <- getRank(x['nome']) %>%
                              mutate(ind = x['descriz']) %>%
                              filter(GEO == 'ITH2' & rank>=7)
                      }
                  )
    )
    df
}

getComparison <- function(ind) {
    
    df <-  getRank(ind) %>%
        mutate(ind = ind) %>%
        filter(GEO == 'ITH2' | rank %in% c(1,2,11,12)) %>%
        arrange(rank)
    df
}

## Start Comparison Module
## Comparison UI
comparisonOutput <- function(id, label) {
    ns <- NS(id)
    
    tagList(
        box(id = label,
            title = label,
            width = 4,
            collapsible = T,
            collapsed = F,
            solidHeader = T,

            tableOutput(ns("table"))
        )
    )
}

## Comparison Server
comparison <- function(input, output, session, ind) {
    output$table <- renderTable({
        getComparison(ind) %>%
            select(Rank = rank, Geo = GEO, Valore = obsValue)
    },
    include.rownames = F)
}
## End Comparison Module