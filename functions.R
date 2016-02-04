library(rsdmx)
library(dplyr)
library(tidyr)
library(RODBC)
library(memoise)
library(DT)
library(rCharts)


source("password.R")

## Get the concepts (filters) relative to the key DataFlow from EUROSTAT
downloadConcepts <- function(key) {
    BaseUrl <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_"
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    concepts <- as.data.frame(DSD@concepts, 
                              conceptSchemeId = paste0("CS_DSD_", key))
    return(concepts)
}

## Get the code list relative to the concept from EUROSTAT
downloadCodeList <- function(key, concept) {
    BaseUrl <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_"
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    codelist <- as.data.frame(DSD@codelists, 
                              codelistId = paste0("CL_", concept))
    return(codelist)
}

## Get the data, given the filter
downloadData <- function(key, filter = NULL) {
    BaseUrl <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data"
    if (is.null(filter)) {
        nFilter <- nrow(getConcepts(key)) - 4
        filter <- paste(rep(".", nFilter - 1), collapse = "")
    }
    dataUrl <- paste(BaseUrl, 
                     key, 
                     filter, 
                     sep = "/")
    data <- as.data.frame(readSDMX(dataUrl))
    
    write.csv2(data, paste0('backupData/', key, '.csv'), row.names = F)
    return(data)
}

getDataOffline <- function(key) {
    data <- read.csv2(paste0('backupData/', key, '.csv'), 
                      stringsAsFactors = F,
                      quote = '"')
    # read.csv2 trasforma le colonne 'T' in booleane
    # quindi li ritrasformiamo in character
    # (non che quelle colonne siano utilizzate)
    data[,sapply(data,class) == "logical"] <-
        sapply(data[,sapply(data,class) == "logical"],
               function(i) substr(as.character(i),1,1)
        )
    return(data)
}

getData <- function(key, filter = NULL) {
    file <- paste0('backupData/', key, '.csv')
    old <- difftime(Sys.time(), file.info(file)$mtime, units = "days") > 10
    if (file.exists(file) & !old)  data <- getDataOffline(key)
    else data <- downloadData(key, filter)
    return(data)
}

## Transform the data to have vertical year and horizontal Geo
pivotData <- function(x) {
    if (all(c("GEO", "obsTime", "obsValue") %in% names(x))) {
        res <- x %>% 
            select(GEO, obsTime, obsValue) %>% 
            spread(obsTime, obsValue) %>% 
            .[, c(1, (ncol(.) - 9):ncol(.))] %>% 
            mutate(
                img = ifelse(
                    .[, 11] > .[, 10], 
                    '<img src="uparrow137.svg" height="16" width = "16"></img>', 
                    '<img src="downarrow103.svg" height="16" width = "16"></img>'
                )
            ) %>%
            arrange(GEO)
        
        return(res)
    } else cat("c'è stato qualche errore")
}


## Helper to obtain the string to insert (manually) in the DB
getConceptsForSQL <- function(key) {
    lev <- levels(getConcepts(key)$id)
    a <- head(lev, -6) %>% 
        paste(collapse = ".")
    return(a)
}

## Retrive the id starting from the name
getId <- function(key) {
    conn <- myConnection()
    id <- sqlQuery(
        conn, 
        paste0("SELECT idDataFlow FROM tabIndicatori WHERE nome = '", 
               key, 
               "'"), 
        stringsAsFactors = F)
    odbcClose(conn)
    return(id)
}

## Retrive the filter values
getFilters <- function(id) {
    conn <- myConnection()
    concepts <- sqlQuery(
        conn, 
        paste0("SELECT value FROM tabConcepts WHERE idDataFlow = '", 
               id,  
               "'"), 
        stringsAsFactors = F)
    filter <- paste0(unlist(concepts), collapse = ".")
    odbcClose(conn)
    return(filter)
}

## Retive sector (fixed)

getSectors <- function() {
    conn <- myConnection()
    sectors <- sqlQuery(
        conn, 
        "SELECT descriz, idSettore FROM tabSettori", 
        stringsAsFactors = F)
    odbcClose(conn)
    options <- list()
    for (i in 1:nrow(sectors)) {
        options[[as.character(sectors$descriz[i])]] <- sectors$idSettore[i]
    }
    
    return(options)
}

getGEOFilters <- function() {
    conn <- myConnection()
    filters <- sqlQuery(conn, "SELECT id FROM tabNUTS", 
                        stringsAsFactors = F) %>%
        unlist() %>%
        paste0(collapse = "+")
    odbcClose(conn)
    return(filters)
}

getGEO <- function() {
    conn <- myConnection()
    filters <- sqlQuery(conn, "SELECT id, descriz, stato FROM tabNUTS", 
                        stringsAsFactors = F) %>%
        transmute(id, 
                  Regione = descriz, 
                  Stato = paste0('<img src="', 
                                 stato, 
                                 '.png" height="24" width = "24" alt = "', 
                                 stato, 
                                 '"></img>'))
    odbcClose(conn)
    return(filters)
}

getIndicators <- function(idSector = NULL) {
    conn <- myConnection()
    query <- 'SELECT * FROM tabIndicatori'
    query <- ifelse(
        is.null(idSector), 
        query, 
        paste0(query, ' WHERE idSettore =', idSector)
    )
    indicators <- sqlQuery(
        conn, 
        query, 
        stringsAsFactors = F)
    odbcClose(conn)
    
    return(indicators)
}

getIndicatorsList <- function(idSector = NULL){
    indicators <- getIndicators(idSector)
    options <- list()
    for (i in 1:nrow(indicators)) {
        options[[as.character(indicators$descriz[i])]] <- indicators$nome[i]
    }
    return(options)
}

getIndName <- function(key) {
    conn <- myConnection()
    indicator <- sqlQuery(
        conn,
        paste0("SELECT descriz FROM tabIndicatori WHERE nome ='",
               key,
               "'"),
        stringsAsFactors = F)
    odbcClose(conn)
    
    return(indicator)
}


makeInteractivePlot <- function(data, selected) {
    data <- select(data, GEO, obsValue, obsTime) %>% 
        mutate(obsTime = as.numeric(obsTime))
    
    p <- hPlot(y = "obsValue", 
               x = "obsTime", 
               data = data, 
               type = "line", 
               group = "GEO", 
               radius = 0)
    p$chart(zoomType = "xy")
    p$xAxis(categories = data$obsTime,
            title = list(text = "Anno")
    )
    p$yAxis(title = list(text = "Valore", 
                         format = "{point.y:,.0f}")
    )
    
    
    
    selected <- pivotData(data)[selected, 1]
    # Black Magic
    p$params$series = lapply(seq_along(p$params$series), function(i) {
        x = p$params$series[[i]]
        x$visible = x$name %in% selected
        return(x)
    })
    return(p)
} 

makeText <- function(data) {
    dataTN <- data %>%
        filter(GEO == 'ITH2') %>%
        arrange(desc(obsTime))
    uAnno <- slice(dataTN ,which.max(obsTime))
    pAnno <- slice(dataTN, which.min(obsTime))
    paste('Ultimo anno TN:', 
          uAnno$obsValue, 
          '<br> Primo anno TN :', 
          pAnno$obsValue)
}

getWholeTNData <- function() {
    indicators <- getIndicators()$nome
    
    df <- lapply(indicators, FUN = function(x) getTNData(x))
    df <- Reduce(function(...) merge(..., by = 'obsTime', all = T), df)
    colnames(df) <- c('obsTime', indicators)
    return(df)
}

getTNData <- function(key) {
    id <- getSQLId(key)
    filter <- getFilters(id)
    geoFilter <- 'ITH2'
    
    data <- getData(key, paste0(filter, '.', geoFilter, '.')) %>%
        select(obsTime, obsValue)
    return(data)
}


getWholeLastData <- memoise(function(updateProgress = NULL) {
    
    indicators <- getIndicators()$nome
    ls <- lapply(indicators, FUN = function(x) getLastData(x, updateProgress))
    
    if (is.function(updateProgress)) updateProgress(detail = 'Merging..')
    df <- Reduce(function(...) merge(..., by = 'GEO', all = T), ls)
    colnames(df) <- c('GEO', indicators)
    
    if (is.function(updateProgress)) updateProgress(detail = 'Done')
    
    df <- data.frame(df,
                     rank = mutate_each(df[-1], funs(min_rank)))

    return(df)
})

getLastData <- function(key, updateProgress = NULL) {
    id <- getSQLId(key)
    filter <- getFilters(id)
    geoFilter <- getGEOFilters()
    
    data <- getData(key, paste0(filter, '.', geoFilter, '.')) %>%
        group_by(GEO) %>%
        filter(obsTime == max(obsTime)) %>%
        select(GEO, obsValue)
    
    if (is.function(updateProgress)) {
        updateProgress(detail = key)
    }
    
    return(data)
}

