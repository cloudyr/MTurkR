QualificationRequestsToDataFrame <-
function (xml = NULL, xml.parsed = NULL){
    if(!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    requests.xml <- xpathApply(xml.parsed, "//QualificationRequest", function(x){
        children <- xmlChildren(x)
        return(list(
            QualificationRequestId = xmlValue(children$QualificationRequestId),
            
        ))
    })
    return(do.call(rbind.data.frame,out))
    
    if(length(requests.xml) > 0) {
        requests <- setNames(data.frame(matrix(nrow = length(requests.xml), ncol = 5)),
                        c("QualificationRequestId", "QualificationTypeId",
                        "SubjectId", "SubmitTime", "Answer"))
        for(i in 1:length(requests.xml)) {
            qreq <- xpathApply(xml.parsed,
                paste("//QualificationRequest[",i,"]/QualificationRequestId",sep = ""))
            if(length(qreq) == 1) 
                requests[i, 1] <- xmlValue(qreq[[1]])
            qual <- xpathApply(xml.parsed,
                paste("//QualificationRequest[",i,"]/QualificationTypeId",sep = ""))
            if(length(qual) == 1) 
                requests[i, 2] <- xmlValue(qual[[1]])
            subj <- xpathApply(xml.parsed,
                paste("//QualificationRequest[", i, "]/SubjectId", sep = ""))
            if(length(subj) == 1) 
                requests[i, 3] <- xmlValue(subj[[1]])
            time <- xpathApply(xml.parsed,
                paste("//QualificationRequest[", i, "]/SubmitTime", sep = ""))
            if(length(time) == 1) 
                requests[i, 4] <- xmlValue(time[[1]])
            answ <- xpathApply(xml.parsed,
                paste("//QualificationRequest[", i, "]/Answer", sep = ""))
            if(length(answ) == 1) 
                requests[i, 5] <- xmlValue(answ[[1]])
        }
        return(list(QualificationRequests = requests))
    }
    else
        return(list(QualificationRequests = NULL))
}
