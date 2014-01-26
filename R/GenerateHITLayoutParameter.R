GenerateHITLayoutParameter <-
function (names, values) {
    if(!length(names) == length(values)) 
        stop("length(names) must equal length(values)")
    values <- gsub('&(?!(?:apos|quot|[gl]t|amp);|#)','&amp;', values, perl=TRUE)
    parameter <- ""
    for(i in 1:length(names)) {
        parameter <- paste(parameter, "&HITLayoutParameter.", i,
                            ".Name=", names[i],
                            "&HITLayoutParameter.", i, 
                            ".Value=", curlEscape(values[i]), sep = "")
    }
    return(parameter)
}
