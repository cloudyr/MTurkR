readlogfile <-
function (filename = NULL)
{
    if (is.null(filename)) 
        filename <- "MTurkRlog.tsv"
    if (!filename %in% list.files()) 
        stop("No Log File Found")
	logfile <- read.delim(filename, header = TRUE, sep = "\t", 
		quote = "", stringsAsFactors = FALSE)
	#class(logfile) <- c("data.frame","MTurkRlog")
	invisible(logfile)
}
