.onLoad <- function(libname, pkgname){
	options(MTurkR.logdir = getwd()) # MTurkRlog.tsv directory
	options(MTurkR.sandbox = FALSE) # sandbox logical
}