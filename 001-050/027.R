sieve <- readRDS("helpers/prime_sieve.rds")
best <- 0

# b must be prime and positive for n=0 to produce a prime
# for n=1 to produce a prime, 1+a+b must be prime
# therefore a = prime - b - 1
# we can now iterate over only a, b combos that produce at least two primes

best_a <- NULL
best_b <- NULL

for(b in sieve[sieve < 1000]) {
    for(a in sieve[sieve < 2000] - b - 1) {
        
        gen <- (0:100)^2 + a*(0:100) + b
        n_primes <- which(!(gen > 1 & gen %in% sieve))[1] - 1
        
        if(n_primes > best) {
            print(sprintf("%s/%s/%s", a, b, n_primes))
            best <- n_primes
            best_a <- a
            best_b <- b
        }
    }
}

best_a * best_b
