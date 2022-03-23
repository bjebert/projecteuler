sieve <- readRDS("helpers/prime_sieve.rds")


factors <- function(x) {
    
    N <- 1:floor(sqrt(x))    
    lower <- N[x %% N == 0]
    
    return(sort(unique(c(lower, x / lower))))
}


is_prime <- function(x) {
    return(length(factors(x)) == 2)
}


is_prime_sieve <- function(x) {
    filt <- sieve[sieve <= floor(sqrt(x))]
    
    if(max(sieve) < floor(sqrt(x))) {  # Sieve not big enough
        filt <- c(filt, (max(filt) + 1):(floor(sqrt(x))))
    }
    
    return(all(x %% filt != 0))
}


