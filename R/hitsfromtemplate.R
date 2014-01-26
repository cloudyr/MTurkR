hitsfromtemplate <-
function (template, input, filenames = NULL, write.files = FALSE) {
    template <- readLines(template, warn = FALSE)
    HITs <- list()
    if(!length(grep("\\$\\{", template)) == dim(input)[2]) 
        stop("Number of input variables does not match variables in template")
    if(!is.null(filenames)) {
        if (!length(filenames) == dim(input)[1]) 
            stop("Number of inputs != length(filenames)")
    }
    for(j in 1:dim(input)[1]) {
        newhit <- template
        lines <- grep("\\$\\{", template)
        var <- data.frame(matrix(nrow = length(lines), ncol = 4))
        names(var) <- c("line", "start", "end", "name")
        var$line <- lines
        for(i in 1:length(var$line)) {
            var[i, 2] <- regexpr("\\$\\{", template[var$line[i]])[1]
            var[i, 3] <- regexpr("\\}", template[var$line[i]])[1]
            var[i, 4] <- substring(template[var$line[i]], var$start[i] + 
                2, var$end[i] - 1)
            newhit[var$line[i]] <- paste(strsplit(template[var$line[i]], 
                paste("\\$\\{", var$name[i], "\\}", sep = ""))[[1]][1], 
                input[j, var$name[i]], strsplit(template[var$line[i]], 
                  paste("\\$\\{", var$name[i], "\\}", sep = ""))[[1]][2], 
                sep = "")
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
