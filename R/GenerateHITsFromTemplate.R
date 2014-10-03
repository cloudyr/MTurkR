GenerateHITsFromTemplate <-
function (template, input, filenames = NULL, write.files = FALSE) {
    if(!grepl("\\$\\{.+\\}", template))
        template <- readLines(template, warn = FALSE)
    HITs <- list()
    if(!length(grep("\\$\\{", template)) == ncol(input)) 
        stop("Number of input variables does not match variables in template")
    if(!is.null(filenames)) {
        if(!length(filenames) == dim(input)[1]) 
            stop("Number of inputs != length(filenames)")
    }
    for(j in 1:nrow(input)) {
        newhit <- template
        for(i in 1:ncol(input)) {
            newhit <- gsub(paste0("\\$\\{",names(input)[i],"\\}"), input[j,i], newhit)
        }
        if(write.files == TRUE & is.null(filenames)) 
            writeLines(newhit, paste("NewHIT", j, ".html", sep = ""))
        else if(write.files == TRUE & !is.null(filenames)) 
            writeLines(newhit, filenames[j])
        else
            HITs[[j]] <- newhit
    }
    return(HITs)
}
