QualificationRequirementsToDataFrame <-
function (xml = NULL, xml.parsed = NULL, xmlnodeset = NULL, hit = NULL, 
    hit.number = NULL, sandbox = getOption('MTurkR.sandbox')){
    if(is.null(xmlnodeset) & is.null(xml.parsed) & is.null(xml)) 
        stop("Must supply XML (parsed or unparsed) xor XMLNodeSet")
    batch <- function(xmlnodeset) {
        quals <- setNames(data.frame(matrix(nrow = length(xmlnodeset), ncol = 6)),
                    c("HITId", "QualificationTypeId", "Name",
                    "Comparator", "Value", "RequiredToPreview"))
        for(i in 1:length(xmlnodeset)) {
            quals$QualificationTypeId[i] <- xmlValue(xmlChildren(xmlnodeset[[i]])$QualificationTypeId)
            if(quals$QualificationTypeId[i] %in% ListQualificationTypes()$QualificationTypeId) {
                qlist <- ListQualificationTypes()
                quals$Name[i] <- qlist[qlist$QualificationTypeId == 
                  quals$QualificationTypeId[i], "Qualification"]
            }
            else
                quals$Name[i] <- NA
            quals$Comparator[i] <- xmlValue(xmlChildren(xmlnodeset[[i]])$Comparator)
            if("LocaleValue" %in% names(xmlChildren(xmlnodeset[[i]]))) 
                quals$Value[i] <- xmlValue(xmlChildren(xmlnodeset[[i]])$LocaleValue)
            if("IntegerValue" %in% names(xmlChildren(xmlnodeset[[i]]))) 
                quals$Value[i] <- xmlValue(xmlChildren(xmlnodeset[[i]])$IntegerValue)
            quals$RequiredToPreview[i] <- xmlValue(xmlChildren(xmlnodeset[[i]])$RequiredToPreview)
        }
        return(quals)
    }
    if(!is.null(xmlnodeset))
        return(batch(xmlnodeset))
    else if(!is.null(xml.parsed) | !is.null(xml)) {
        if(!is.null(xml)) 
            xml.parsed <- xmlParse(xml)
        if(!is.null(hit.number)) 
            xmlnodeset <- xpathApply(xml.parsed, paste("//HIT[", 
                hit.number, "]/QualificationRequirement", sep = ""))
        else if(is.null(hit.number)) 
            xmlnodeset <- xpathApply(xml.parsed, "//QualificationRequirement")
        if(!is.null(xmlnodeset)) 
            return(batch(xmlnodeset))
        else
			return(NULL)
    }
    else if(!is.null(hit)) {
        xmlnodeset <- xpathApply(xmlParse(GetHIT(hit = hit, keypair = credentials, sandbox = sandbox)$xml), 
            paste("//QualificationRequirement", sep = ""))
        return(batch(xmlnodeset))
    }
    else
		return(NULL)
}
