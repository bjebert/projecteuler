source("helpers/primes.R")

primes_filt <- sieve[ceiling(log(sieve + 1, base = 10)) <= 7]


K <- 1
biggest_K <- NA
biggest_prime <- NA

while(TRUE) {
    consecutive_sums <- rowSums(sapply(0:K, function(N) shift(primes_filt, N)))
    consecutive_sums <- consecutive_sums[!is.na(consecutive_sums)]
    
    if(min(consecutive_sums) > max(primes_filt)) {
        break
    }
    
    matching_primes <- consecutive_sums[consecutive_sums %in% primes_filt]
    
    if(length(matching_primes) >= 1) {
        biggest_K <- K + 1
        biggest_prime <- matching_primes[1]
        
        print(sprintf("K = %d, prime = %d", biggest_K, biggest_prime))
    }
    
    K <- K + 1
}








