QualificationRequestsToDataFrame <-
function (xml = NULL, xml.parsed = NULL){
    if(!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    out <- xpathApply(xml.parsed, "//QualificationRequest", function(x){
        children <- xmlChildren(x)
        return(list(
            QualificationRequestId = xmlValue(children$QualificationRequestId),
            QualificationTypeId = xmlValue(children$QualificationTypeId),
            SubjectId = xmlValue(children$SubjectId),
            SubmitTime = xmlValue(children$SubmitTime),
            Answer = xmlValue(children$Answer)
        ))
    })
    if(!is.null(out))
        return(list(QualificationRequests = out))
    else
        return(list(QualificationRequests = NULL))
}
