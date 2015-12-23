library(rsdmx)


# allDataUrl <- 'http://www.ec.europa.eu/eurostat/SDMX/diss-web/rest/dataflow/ESTAT/all/latest'
# 
# 
# estat <- findSDMXServiceProvider('ESTAT')
# 
# allData <- readSDMX(allDataUrl)
# testData <- readSDMX(provider = estat, key = 'demo_r_d3area', operation = 'data', filter = c('', '', 'TOTAL', 'ITH4+ITH2', ''), start = 2005, end = 2011)
# 

getConcepts <- function(key) {
    BaseUrl <- 'http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_'
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    concepts <- as.data.frame(DSD@concepts)
    return(concepts)
}

getCodeList <- function(key, concept) {
    BaseUrl <- 'http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_'
    DSDUrl <- paste0(BaseUrl, key)
    DSD <- readSDMX(DSDUrl)
    codelist <- as.data.frame(DSD@codelists,codelistId = concept)
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

# conc <- getConcepts('demo_r_d3area')
# codel <- getCodeList(key, 'UNIT')
