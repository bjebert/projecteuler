source("helpers/primes.R")

filt <- sieve[!(sieve %in% c(2, 5)) & sieve < 1e4]
compatible <- setNames(lapply(1:length(filt), function(x) rep(NA, length(filt))), filt)


is_compatible <- function(a, b) {
    a != b && is_prime(as.numeric(paste0(a, b))) && is_prime(as.numeric(paste0(b, a)))
}
    

get_compatible <- function(x) {
    wx <- which(x == filt)
    xc <- as.character(x)
    w <- which(is.na(compatible[[xc]]))
    
    for(i in w) {
        compatible_check <- is_compatible(x, filt[i]) 
        
        compatible[[xc]][i] <<- compatible_check
        compatible[[as.character(filt[i])]][wx] <<- compatible_check
    }
    
    return(filt[compatible[[xc]]])
}


Q <- as.list(filt)
set_size <- 5
lowest <- Inf

while(length(Q) > 0) {
    curr <- Q[[1]]
    Q <- Q[-1]
    # print(sprintf("%d / %d", length(Q), length(curr)))
    
    if(length(curr) == set_size) {
        if(sum(curr) < lowest) {
            lowest <- sum(curr)
            print(sprintf("%s = %s", lowest, paste(curr, collapse = "/")))
        }
        
        next
    }
    
    n <- Reduce(intersect, lapply(curr, get_compatible))
    
    # Add all compatible to current to queue
    if(length(n) >= 1) {
        Q <- c(lapply(n, function(x) c(curr, x)), Q)
    }
    
    # Sort Q by mean number
    Q <- Q[order(-sapply(Q, length), sapply(Q, mean))]
}

