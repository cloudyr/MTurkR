QualificationTypesToDataFrame <-
function (xml = NULL, xml.parsed = NULL) {
    if(!is.null(xml)) 
        xml.parsed = xmlParse(xml)
    total <- length(xpathApply(xml.parsed, "//QualificationTypeId"))
    quals <- as.data.frame(matrix(nrow = total, ncol = 13))
    names(quals) <- c("QualificationTypeId", "CreationTime", 
        "Name", "Description", "Keywords", "QualificationTypeStatus", 
        "AutoGranted", "AutoGrantedValue", "IsRequestable", "RetryDelayInSeconds", 
        "TestDurationInSeconds", "Test", "AnswerKey")
    for(i in 1:total) {
        q <- xpathApply(xml.parsed, "//QualificationType")[[i]]
        quals[i, 1] <- xmlValue(xmlChildren(q)$QualificationTypeId)
        quals[i, 2] <- xmlValue(xmlChildren(q)$CreationTime)
        quals[i, 3] <- xmlValue(xmlChildren(q)$Name)
        quals[i, 4] <- xmlValue(xmlChildren(q)$Description)
        quals[i, 5] <- xmlValue(xmlChildren(q)$Keywords)
        quals[i, 6] <- xmlValue(xmlChildren(q)$QualificationTypeStatus)
        quals[i, 7] <- xmlValue(xmlChildren(q)$AutoGranted)
        quals[i, 8] <- xmlValue(xmlChildren(q)$AutoGrantedValue)
        quals[i, 9] <- xmlValue(xmlChildren(q)$IsRequestable)
        quals[i, 10] <- xmlValue(xmlChildren(q)$RetryDelayInSeconds)
        quals[i, 11] <- xmlValue(xmlChildren(q)$TestDurationInSeconds)
        quals[i, 12] <- xmlValue(xmlChildren(q)$Test)
        quals[i, 13] <- xmlValue(xmlChildren(q)$AnswerKey)
    }
    return(quals)
}
