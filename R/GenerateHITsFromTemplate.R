GenerateHITsFromTemplate <-
function (template, input, filenames = NULL, write.files = FALSE) {
    template <- readLines(template, warn = FALSE)
    HITs <- list()
    if(!length(grep("\\$\\{", template)) == dim(input)[2]) 
        stop("Number of input variables does not match variables in template")
    if(!is.null(filenames)) {
        if(!length(filenames) == dim(input)[1]) 
            stop("Number of inputs != length(filenames)")
    }
    for(j in 1:dim(input)[1]) {
        newhit <- template
        lines <- grep("\\$\\{", template)
        vars <- setNames(data.frame(matrix(nrow = length(lines), ncol = 4)),
                c("line", "start", "end", "name"))
        vars$line <- lines
        for(i in 1:length(vars$line)) {
            var[i, 2] <- regexpr("\\$\\{", template[vars$line[i]])[1]
            var[i, 3] <- regexpr("\\}", template[vars$line[i]])[1]
            var[i, 4] <- substring(template[vars$line[i]], vars$start[i] + 
                2, vars$end[i] - 1)
            newhit[vars$line[i]] <- paste(strsplit(template[vars$line[i]], 
                paste("\\$\\{", vars$name[i], "\\}", sep = ""))[[1]][1], 
                input[j, vars$name[i]], strsplit(template[vars$line[i]], 
                  paste("\\$\\{", vars$name[i], "\\}", sep = ""))[[1]][2], 
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
