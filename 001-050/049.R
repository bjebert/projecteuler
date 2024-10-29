source("helpers/primes.R")

max_m <- 4500

primes4 <- sieve[ceiling(log(sieve, base = 10)) == 4]
primes4_mat <- t(sapply(strsplit(as.character(primes4), ""), as.numeric))


for(i in 1:nrow(primes4_mat)) {
    
    perm_raw <- primes4_mat[sapply(1:nrow(primes4_mat), function(j) {
        identical(sort(primes4_mat[j,]), sort(primes4_mat[i,]))
    }),]
    
    if(is.matrix(perm_raw)) {
        prime_permutations <- as.numeric(apply(perm_raw, 1, function(x) paste(x, collapse = "")))
    } else {
        prime_permutations <- as.numeric(paste(perm_raw, collapse = ""))
    }
    
    if(length(prime_permutations) < 3) {
        next
    }
    
    # look for a 3-digit sequence that can be made with a common m
    
    res <- sapply(1:(length(prime_permutations) - 2), function(j) {
        seq_inc <- prime_permutations[(j+1):length(prime_permutations)] - prime_permutations[j]    
        
        w <- sapply(seq_inc, function(m) all(c(prime_permutations[j], prime_permutations[j] + m, prime_permutations[j] + 2*m) %in% prime_permutations))
        
        if(any(w)) {
            print(prime_permutations[j] + (0:2)*seq_inc[w])
        }
    })
}


