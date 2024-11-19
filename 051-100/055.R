# 055: Lychrel Numbers

library(gmp)


is_palindromic <- function(n) {
    ss <- strsplit(as.character(n), "")[[1]]
    identical(ss, rev(ss))
}


is_lychrel <- function(n) {
    for(i in 1:50) {
        n_rev <- gmp::as.bigz(as.numeric(paste(rev(strsplit(as.character(n), "")[[1]]), collapse = "")))
        if(is_palindromic(n + n_rev)) {
            return(FALSE)
        }
        
        n <- gmp::as.bigz(n + n_rev)
    }
    
    return(TRUE)
}


sum(sapply(1:10000, is_lychrel))
