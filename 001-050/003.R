factors <- function(x) {
    
    N <- 1:floor(sqrt(x))    
    lower <- N[x %% N == 0]
    
    return(sort(unique(c(lower, x / lower))))
}


is_prime <- function(x) {
    return(length(factors(x)) == 2)
}


num_factors <- factors(600851475143)
max(num_factors[sapply(num_factors, is_prime)])

