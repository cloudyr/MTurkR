BonusPaymentsToDataFrame <-
function (xml = NULL, xml.parsed = NULL){
    if(!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    bonus.xml <- xpathApply(xml.parsed, "//BonusPayment")
    if(length(bonus.xml) > 0) {
        bonuses <- setNames(data.frame(matrix(nrow = length(bonus.xml), ncol = 7)),
                    c("AssignmentId", "WorkerId", "Amount", "CurrencyCode",
                    "FormattedPrice", "Reason", "GrantTime"))
        for(i in 1:length(bonus.xml)) {
            bonuses[i, ] <- c(    xmlValue(xpathApply(xml.parsed, "//AssignmentId")[[i]]),
                                xmlValue(xpathApply(xml.parsed, "//WorkerId")[[i]]),
                                xmlValue(xpathApply(xml.parsed, "//Amount")[[i]]),
                                xmlValue(xpathApply(xml.parsed, "//CurrencyCode")[[i]]),
                                xmlValue(xpathApply(xml.parsed, "//FormattedPrice")[[i]]),
                                xmlValue(xpathApply(xml.parsed, "//Reason")[[i]]),
                                xmlValue(xpathApply(xml.parsed, "//GrantTime")[[i]]))
        }
        return(bonuses)
    }
    else
        return(NULL)
}
