ParseErrorCodes <-
function (xml = NULL, xml.parsed = NULL) 
{
    if (!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    xml.errors <- xpathApply(xml.parsed, "//Error")
    errors <- data.frame(matrix(nrow = length(xml.errors), ncol = 2))
    names(errors) <- c("Code", "Message")
    for (i in 1:length(xml.errors)) {
        errors[i, 1] <- xmlValue(xpathApply(xml.errors[[i]], 
            "//Code")[[1]])
        errors[i, 2] <- xmlValue(xpathApply(xml.errors[[i]], 
            "//Message")[[1]])
    }
    invisible(errors)
}
