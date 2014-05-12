QualificationTypesToDataFrame <-
function (xml = NULL, xml.parsed = NULL) {
    if(!is.null(xml)) 
        xml.parsed = xmlParse(xml)
    out <- xpathApply(xml.parsed, "//QualificationType", function(x){
        children <- xmlChildren(x)
        return(list(
            QualificationTypeId = xmlValue(children$QualificationTypeId),
            CreationTime = xmlValue(children$CreationTime),
            Name = xmlValue(children$Name),
            Description = xmlValue(children$Description),
            Keywords = xmlValue(children$Keywords),
            QualificationTypeStatus = xmlValue(children$QualificationTypeStatus),
            AutoGranted = xmlValue(children$AutoGranted),
            AutoGrantedValue = xmlValue(children$AutoGrantedValue),
            IsRequestable = xmlValue(children$IsRequestable),
            RetryDelayInSeconds = xmlValue(children$RetryDelayInSeconds),
            TestDurationInSeconds = xmlValue(children$TestDurationInSeconds),
            Test = xmlValue(children$Test),
            AnswerKey = xmlValue(children$AnswerKey)
        ))
    })
    return(do.call(rbind.data.frame,out))
}
