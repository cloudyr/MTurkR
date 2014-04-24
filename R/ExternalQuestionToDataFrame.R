ExternalQuestionToDataFrame <-
function (xml = NULL, xml.parsed = NULL) {
    if(!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    removeXMLNamespaces(xml.parsed, all = TRUE)
    if(length(xmlChildren(xml.parsed)) > 0) {
        external.url <- xmlValue(xmlChildren(xml.parsed)$ExternalURL)
        frame.height <- xmlValue(xmlChildren(xml.parsed)$FrameHeight)
        return(list(external.url = external.url, frame.height = frame.height))
    }
    else
        return(list(external.url = NULL, frame.height = NULL))
}
