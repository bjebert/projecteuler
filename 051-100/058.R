source("helpers/primes.R")
library(memoise)


# Create corners ----------------------------------------------------------

N <- 50000  # side length = N*2 - 1
corners <- list(1, c(3, 5, 7, 9))

# Each additional spiral, each corner val will increase by +8 from its last increase
for(i in 3:ceiling(N / 2)) {
    corners[[i]] <- corners[[i-1]] + c(2, 4, 6, 8) + (i-2) * 8
}


# Check primality ---------------------------------------------------------

corner_is_prime <- lapply(corners, function(x) sapply(x, function(y) is_prime_sieve(y, sieve)))

corner_prime_sums <- sapply(corner_is_prime, sum)
prime_ratio <- cumsum(corner_prime_sums) / seq(from = 1, by = 4, length.out = length(corner_prime_sums))

w <- which(prime_ratio < 0.1)[2]

w*2 - 1
