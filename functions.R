library(rsdmx)


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
    codelist <- as.data.frame(DSD@codelists, codelistId = concept)
    return(codelist)
}

getData <- function(key, filter = NULL) {
    BaseUrl <- 'http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data'
    nFilter <- nrow(getConcepts(key)) - 4
    if (is.null(filter)) {
        filter <- paste(rep('.', nFilter - 1), collapse = '')
        }
    dataUrl <- paste(BaseUrl, key, filter, sep ='/')
    data <- as.data.frame(readSDMX(dataUrl))
    return(data)
}
