HTMLQuestionToDataFrame <-
function (xml = NULL, xml.parsed = NULL) 
{
    if (!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    removeXMLNamespaces(xml.parsed, all = TRUE)
    if (length(xmlChildren(xml.parsed)) > 0) {
        html.content <- xmlValue(xmlChildren(xml.parsed)$HTMLContent)
        frame.height <- xmlValue(xmlChildren(xml.parsed)$FrameHeight)
        return(list(html.content = html.content, frame.height = frame.height))
    }
    else
		return(list(html.content = NULL, frame.height = NULL))
}
