GenerateHITsFromTemplate <-
function (template, input, filenames = NULL, write.files = FALSE) {
    if (!grepl("\\$\\{.+\\}", template)) {
        template <- readLines(template, warn = FALSE)
    }
    template <- paste0(template, collapse = "\n")
    HITs <- list()
    if (!is.null(filenames)) {
        if (!length(filenames) == dim(input)[1]) {
            stop("Number of inputs != length(filenames)")
        }
    }
    for (j in 1:nrow(input)) {
        newhit <- template
        for (i in 1:ncol(input)) {
            newhit <- gsub(paste0("\\$\\{",names(input)[i],"\\}"), input[j,i], newhit)
        }
        if (write.files == TRUE) {
            if (is.null(filenames)) {
                writeLines(newhit, paste("NewHIT", j, ".html", sep = ""))
            } else {
                writeLines(newhit, filenames[j])
            }
        }
        HITs[[j]] <- newhit
    }
    return(HITs)
}
