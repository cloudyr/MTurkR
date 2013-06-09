QualificationsToDataFrame <-
function (xml = NULL, xml.parsed = NULL) 
{
    if (!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    quals.xml <- xpathApply(xml.parsed, "//Qualification")
    if (length(quals.xml) > 0) {
        quals <- data.frame(matrix(nrow = length(quals.xml), 
            ncol = 5))
        names(quals) <- c("QualificationTypeId", "WorkerId", 
            "GrantTime", "Value", "Status")
        for (i in 1:length(quals.xml)) {
            if ("IntegerValue" %in% names(xmlChildren(quals.xml[[1]]))) 
                value.type <- "IntegerValue"
            if ("LocaleValue" %in% names(xmlChildren(quals.xml[[1]]))) 
                value.type <- "LocaleValue"
            qual <- xpathApply(xml.parsed, paste("//Qualification[", 
                i, "]/QualificationTypeId", sep = ""))
            if (length(qual) == 1) 
                quals[i, 1] <- xmlValue(qual[[1]])
            subj <- xpathApply(xml.parsed, paste("//Qualification[", 
                i, "]/SubjectId", sep = ""))
            if (length(subj) == 1) 
                quals[i, 2] <- xmlValue(subj[[1]])
            time <- xpathApply(xml.parsed, paste("//Qualification[", 
                i, "]/GrantTime", sep = ""))
            if (length(time) == 1) 
                quals[i, 3] <- xmlValue(time[[1]])
            valu <- xpathApply(xml.parsed, paste("//Qualification[", 
                i, "]/", value.type, sep = ""))
            if (length(valu) == 1) 
                quals[i, 4] <- xmlValue(valu[[1]])
            stat <- xpathApply(xml.parsed, paste("//Qualification[", 
                i, "]/Status", sep = ""))
            if (length(stat) == 1) 
                quals[i, 5] <- xmlValue(stat[[1]])
        }
        return(Qualifications = quals)
    }
    else
		return(list(Qualifications = NULL))
}
