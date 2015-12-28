library(rsdmx)
library(dplyr)
library(tidyr)


getConcepts <- function(key) {
    BaseUrl <- 'http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_'
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    concepts <- as.data.frame(DSD@concepts, conceptSchemeId = paste0('CS_DSD_',key))
    return(concepts)
}

getCodeList <- function(key, concept) {
    BaseUrl <- 'http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_'
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    codelist <- as.data.frame(DSD@codelists, codelistId = paste0('CL_',concept))
    return(codelist)
}

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

pivotData <- function(x) {
    if (all(c('GEO', 'obsTime', 'obsValue') %in% names(x))) {
        res <- x %>%
            select(GEO, obsTime, obsValue) %>%
            spread(GEO, obsValue)
        return(res)
    }
    else cat('c\'è stato qualche errore')
}

displayTab <- function(key, filter = NULL) {
    data <- getData(key, filter) %>%
        pivotData()
    return(data)
}

getConceptsForSQL <- function(key) {
    lev <- levels(getConcepts(key)$id)
    a <- head(lev, -6) %>%
        paste(collapse = '.')
    return(a)
}