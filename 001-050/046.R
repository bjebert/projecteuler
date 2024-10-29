source("helpers/primes.R")


generate_odd_composites <- function(lo = 2, hi) {
    (lo:hi)[sapply(lo:hi, function(i) {
        i %% 2 != 0 && !is_prime(i)
    })]  
}


is_goldbach_sum <- function(x, sieve, twice_squares) {
    rng1 <- sieve[sieve < x]
    rng2 <- twice_squares[twice_squares < x]
    
    return(any((x - rng2) %in% rng1))
}


twice_squares <- 2*(1:1000)^2
composites <- generate_odd_composites(2, 10000)


for(x in composites) {
    if(!is_goldbach_sum(x, sieve, twice_squares)) {
        print(x)
        break
    }
}




