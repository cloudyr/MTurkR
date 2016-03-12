emptydf <- function(nrow, ncol, names) {
    setNames(data.frame(matrix(nrow = nrow, ncol = ncol), stringsAsFactors = FALSE), names)
}
