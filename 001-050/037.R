source("helpers/primes.R")
sieve <- sieve[sieve > 10]

sc <- strsplit(as.character(sieve), "")

# Remove any 3 digit (0, 2, 4, 5, 6, 8)-containing primes from sieve; as when truncated, will not result in a prime at some point
filt <- sc[sapply(sc, function(x) !(any(c("0", "2", "4", "5", "6", "8") %in% x))) | sieve < 100]

w <- sapply(filt, function(x) {
    left <- as.numeric(sapply(2:length(x), function(i) paste(x[i:length(x)], collapse = "")))
    right <- as.numeric(sapply(length(x):2, function(i) paste(x[1:(i-1)], collapse = "")))
    
    return(all(sapply(left, is_prime)) && all(sapply(right, is_prime)))
})

sum(as.numeric(sapply(filt[w], paste0, collapse = "")))



