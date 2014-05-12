WorkerBlockToDataFrame <-
function (xml = NULL, xml.parsed = NULL) {
    if(!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    out <- xpathApply(xml.parsed, "//WorkerBlock", function(x){
        children <- xmlChildren(x)
        return(list(
            WorkerId <- xmlValue(children$WorkerId),
            Reason = xmlValue(children$Reason)
        ))
    })
    return(do.call(rbind.data.frame,out))
}
