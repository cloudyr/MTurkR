GenerateHITLayoutParameter <-
function (names, values) {
    if (missing(names) || is.null(names)) {
        n <- names(values)
        if (all(n != "")) {
            names <- n
        } else {
            stop("'names' must be specified or names(values) must be non-empty")
        }
    }
    values <- unname(values)
    if (!length(names) == length(values)) {
        stop("length(names) must equal length(values)")
    }
    values <- gsub('&(?!(?:apos|quot|[gl]t|amp);|#)','&amp;', values, perl=TRUE)
    parameter <- ""
    for (i in 1:length(names)) {
        parameter <- paste(parameter, "&HITLayoutParameter.", i,
                            ".Name=", names[i],
                            "&HITLayoutParameter.", i, 
                            ".Value=", curl_escape(values[i]), sep = "")
    }
    return(parameter)
}
