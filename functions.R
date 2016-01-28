library(rsdmx)
library(dplyr)
library(tidyr)
library(RODBC)
library(DT)
library(rCharts)


source('password.R')

## Get the concepts (filters) relative to the key DataFlow from EUROSTAT
getConcepts <- function(key) {
    BaseUrl <- 'http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_'
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    concepts <- as.data.frame(DSD@concepts, conceptSchemeId = paste0('CS_DSD_',key))
    return(concepts)
}

## Get the code list relative to the concept from EUROSTAT
getCodeList <- function(key, concept) {
    BaseUrl <- 'http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_'
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    codelist <- as.data.frame(DSD@codelists, codelistId = paste0('CL_',concept))
    return(codelist)
}

## Get the data, given the filter
getData <- function(key, filter = NULL) {
    BaseUrl <- 'http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data'
    if (is.null(filter)) {
        nFilter <- nrow(getConcepts(key)) - 4
        filter <- paste(rep('.', nFilter - 1), collapse = '')
    }
    dataUrl <- paste(BaseUrl, key, filter, sep ='/')
    data <- as.data.frame(readSDMX(dataUrl))
    return(data)
}

## Transform the data to have vertical year and horizontal Geo
pivotData <- function(x) {
    if (all(c('GEO', 'obsTime', 'obsValue') %in% names(x))) {
        res <- x %>%
            select(GEO, obsTime, obsValue) %>%
            spread(obsTime, obsValue) %>%
            .[,c(1,(ncol(.)-9):ncol(.))] %>%
            arrange(GEO)
        return(res)
    }
    else cat('c\'è stato qualche errore')
}

## Wrapper to get and transform the data
displayTab <- function(key, filter = NULL) {
    data <- getData(key, filter) %>%
        pivotData()
    return(data)
}

## Helper to obtain the string to insert (manually) in the DB
getConceptsForSQL <- function(key) {
    lev <- levels(getConcepts(key)$id)
    a <- head(lev, -6) %>%
        paste(collapse = '.')
    return(a)
}

## Retrive the id starting from the name
getSQLId <- function(key) {
    conn <- myConnection()
    id <- sqlQuery(conn, paste0('SELECT idDataFlow FROM tabIndicatori WHERE nome = \'', key,'\''), stringsAsFactors = F)
    odbcClose(conn)
    return(id)
}

## Retrive the filter values
getFilters <- function(id) {
    conn <- myConnection()
    concepts <- sqlQuery(conn, paste0('SELECT value FROM tabConcepts WHERE idDataFlow = \'', id, '\''), stringsAsFactors = F)
    filter <- paste0(unlist(concepts), collapse = '.')
    odbcClose(conn)
    return(filter)
}

## Retive sector (fixed)

getSectors <- function() {
    conn <- myConnection()
    sectors <- sqlQuery(conn, 'SELECT descriz, idSettore FROM tabSettori', stringsAsFactors = F)
    odbcClose(conn)
    options <- list()
    for (i in 1:nrow(sectors)) {
        options[[as.character(sectors$descriz[i])]] <- sectors$idSettore[i]
    }
    
    return(options)
}

getGEOFilters <- function() {
    conn <- myConnection()
    filters <- sqlQuery(conn, 'SELECT id FROM tabNUTS', stringsAsFactors = F) %>%
        unlist() %>%
        paste0(collapse = '+')
    odbcClose(conn)
    return(filters)
}

getIndicators <- function(idSector) {
    conn <- myConnection()
    indicators <- sqlQuery(conn, paste0('SELECT * FROM tabIndicatori WHERE idSettore=', idSector), stringsAsFactors = F)
    odbcClose(conn)
    
    options <- list()
    for (i in 1:nrow(indicators)) {
        options[[as.character(indicators$descriz[i])]] <- indicators$nome[i]
    }
    #     options[[as.character(indicators$descriz)]] <- indicators$nome
    return(options)
}

makePlot <- function(data) {
    if (all(c('GEO', 'obsTime', 'obsValue') %in% names(data))) {
        p <- ggplot(data, aes(x = obsTime, y = obsValue, group = GEO, color = GEO)) + geom_smooth(size = 1) + theme_minimal() + theme(axis.text.x = element_text(angle = 45))
        return(p)
    }
}

makeInteractivePlot <- function(data) {
    data <- select(data, GEO, obsValue, obsTime)
    
    p <- hPlot(obsValue ~ obsTime, 
               data = data,
               type = 'line',
               group = 'GEO',
               marker = list(enabled = 'false')
    )
    
    visible = c('ITH1','ITH2')
    
    # Black Magic
    p$params$series = lapply(seq_along(p$params$series), function(i){
        x = p$params$series[[i]]
        x$visible = x$name %in% visible
        return(x)
    })
    return(p)
}

pobj <- makeInteractivePlot(data)
pobj