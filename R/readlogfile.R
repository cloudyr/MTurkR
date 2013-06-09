readlogfile <-
function (filename = NULL, shell = FALSE) 
{
    if (is.null(filename)) 
        filename <- "MTurkRlog.tsv"
    if (!filename %in% list.files()) 
        cat("No Log File Found\n")
    else {
        if (shell == FALSE) {
            logfile <- read.delim(filename, header = TRUE, sep = "\t", 
                quote = "", stringsAsFactors = FALSE)
			#class(logfile) <- c("data.frame","MTurkRlog")
			invisible(logfile)
        }
        else if (shell == TRUE) {
            shell.exec(filename)
        }
    }
}
