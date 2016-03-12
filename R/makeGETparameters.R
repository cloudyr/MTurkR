makeGETparameters <-
function (parameter, value) {
    if (!length(parameter) == length(value)) {
        stop("Parameter and Value have unequal length")
    }
    GET <- ""
    for (i in 1:length(parameter)) {
        GET <- paste(GET, "&", parameter[i], "=", curl_escape(value[i]), sep = "")
    }
    return(GET)
}
