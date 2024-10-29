source("helpers/primes.R")

N <- 4  # desired consecutive length
K <- 2000  # amount extra to calculate each batch

while(TRUE) {
    lo <- K-1999
    fac_out <- sapply(lo:K, function(x) prime_factors(x))
    fac_len <- sapply(fac_out, length)
    
    w <- which(sapply(1:(length(fac_len)-N+1), function(i) {
        return(all(sapply(0:(N-1), function(j) {
            fac_len[i+j] == N
        })))
    }))
    
    if(length(w) > 0) {
        print((lo:K)[w[1]])
        break
    }
 
    K <- K + 2000   
}




