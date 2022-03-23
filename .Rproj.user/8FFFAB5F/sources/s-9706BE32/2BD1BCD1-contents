source("helpers/primes.R")

primes <- c(2, 3, 5, 7, 11, 13)

i <- 14
while(length(primes) < 10001) {
    if(is_prime_sieve(i, primes)) {
        primes <- c(primes, i)
    }
    
    i <- i + 1 
}

saveRDS(primes, "helpers/prime_sieve.rds")

primes[10001]
