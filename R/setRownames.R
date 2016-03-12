setRownames <- function(dat) {
    if (nrow(dat) > 0) {
        return(`row.names<-`(dat, 1:nrow(dat)))
    } else {
        return(dat)
    }
}
