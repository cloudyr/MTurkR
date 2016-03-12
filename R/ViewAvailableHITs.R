ViewAvailableHITs <-
function(query = NULL, requester = NULL, min.reward = NULL, qualified = NULL) {
    parameters <- ""
    if (!is.null(query))  {
        parameters <- paste(parameters, "searchWords=", query, "&", sep = "")
    }
    if (!is.null(requester)) {
        parameters <- paste(parameters, "requesterId=", requester, "&", sep = "")
    }
    if (!is.null(min.reward)) {
        parameters <- paste(parameters, "minReward=", min.reward, "&", sep = "")
    }
    if (!is.null(qualified) && (qualified == TRUE | qualified == "true")){
        parameters <- paste(parameters, "qualifiedFor=on", "&", sep = "")
    }
    if (nchar(parameters) == 0) {
        browseURL("https://www.mturk.com/mturk/searchbar?")
    } else {
        browseURL(paste("https://www.mturk.com/mturk/searchbar?", parameters, sep = ""))
    }
    invisible(NULL)
}
