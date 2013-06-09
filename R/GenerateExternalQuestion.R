GenerateExternalQuestion <-
function (url, frame.height) 
{
    external <- newXMLNode("ExternalQuestion", namespaceDefinitions = "http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2006-07-14/ExternalQuestion.xsd")
    external.url <- newXMLNode("ExternalURL", url, parent = external)
    external.frame <- newXMLNode("FrameHeight", frame.height, 
        parent = external)
    addChildren(external, c(external.url, external.frame))
    string <- toString.XMLNode(external)
    encoded <- curlEscape(string)
    invisible(list(xml.parsed = external, string = string, url.encoded = encoded))
}
