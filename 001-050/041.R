source("helpers/primes.R")
library(combinat)


pandigitals <- rev(sort(unlist(lapply(1:9, function(N) {
    as.numeric(sapply(combinat::permn(1:N), function(x) paste(x, collapse = "")))
}))))


# To speed up prime search, remove any pandigitals ending in 2, 4, 5, 6, 8

pd_filtered <- pandigitals[pandigitals %% 10 %in% c(1, 3, 7, 9)]

for(p in pd_filtered) {
    if(is_prime_sieve(p, sieve)) {
        print(p)
        break
    }
}
