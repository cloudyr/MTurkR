BonusPaymentsToDataFrame <-
function (xml = NULL, xml.parsed = NULL){
    if(!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    out <- xpathApply(xml.parsed, "//BonusPayment", function(x){
        children <- xmlChildren(x)
        bonus <- xmlChildren(children$BonusAmount)
        return(list(
            AssignmentId = xmlValue(children$AssignmentId),
            WorkerId = xmlValue(children$WorkerId),
            Amount = xmlValue(bonus$Amount),
            CurrencyCode = xmlValue(bonus$CurrencyCode),
            FormattedPrice = xmlValue(bonus$FormattedPrice),
            Reason = xmlValue(children$Reason),
            GrantTime = xmlValue(children$GrantTime)
        ))
    })
    return(out)
}
