emptydf <- function(nrow, ncol, names) {
    setNames(data.frame(matrix(NA_character_, nrow = nrow, ncol = ncol), stringsAsFactors = FALSE), names)
}
