QualificationTypesToDataFrame <-
function (xml = NULL, xml.parsed = NULL) {
    if(!is.null(xml)) 
        xml.parsed = xmlParse(xml)
    out <- xpathApply(xml.parsed, "//QualificationType", function(x){
        return(list(
            QualificationTypeId = xmlValue(xmlChildren(x)$QualificationTypeId),
            CreationTime = xmlValue(xmlChildren(x)$CreationTime),
            Name = xmlValue(xmlChildren(x)$Name),
            Description = xmlValue(xmlChildren(x)$Description),
            Keywords = xmlValue(xmlChildren(x)$Keywords),
            QualificationTypeStatus = xmlValue(xmlChildren(x)$QualificationTypeStatus),
            AutoGranted = xmlValue(xmlChildren(x)$AutoGranted),
            AutoGrantedValue = xmlValue(xmlChildren(x)$AutoGrantedValue),
            IsRequestable = xmlValue(xmlChildren(x)$IsRequestable),
            RetryDelayInSeconds = xmlValue(xmlChildren(x)$RetryDelayInSeconds),
            TestDurationInSeconds = xmlValue(xmlChildren(x)$TestDurationInSeconds),
            Test = xmlValue(xmlChildren(x)$Test),
            AnswerKey = xmlValue(xmlChildren(x)$AnswerKey)
        ))
    })
    return(do.call(rbind.data.frame,out))
}
