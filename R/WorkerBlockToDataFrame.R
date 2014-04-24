WorkerBlockToDataFrame <-
function (xml = NULL, xml.parsed = NULL) {
    if(!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    blocks.xml <- xpathApply(xml.parsed, "//WorkerBlock")
    if(!is.null(length(blocks.xml)) && length(blocks.xml) >= 1) {
        workers <- setNames(data.frame(matrix(nrow = length(blocks.xml), ncol = 2)),
                    c("WorkerId", "Reason"))
        for(i in 1:length(blocks.xml)) {
            workers$WorkerId[i] <- xmlValue(xpathApply(xml.parsed, 
                paste("//WorkerBlock[", i, "]/WorkerId", sep = ""))[[1]])
            workers$Reason[i] <- xmlValue(xpathApply(xml.parsed, 
                paste("//WorkerBlock[", i, "]/Reason", sep = ""))[[1]])
        }
        return(workers)
    }
    else
        return(NULL)
}
