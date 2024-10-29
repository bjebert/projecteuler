source("helpers/primes.R")
sieve <- readRDS("helpers/prime_sieve.rds")

small_circ <- c(2, 3, 5, 7)

sc <- strsplit(as.character(sieve), "")

# Filter out any numbers above 10 containing 0,2,4,5,6,8 (as at some circular rotation, they will not be prime)

potential <- sieve[sieve > 10 & sapply(sc, function(x) !(any(c("0", "2", "4", "5", "6", "8") %in% x)))]
pc <- strsplit(as.character(potential), "")

big_circ <- as.numeric(sapply(pc[sapply(pc, function(x) {
    combos <- as.numeric(sapply(1:length(x), function(i) paste(x[(i:(i+length(x)-1) - 1) %% length(x) + 1], collapse = "")))
    all(combos %in% potential)
})], function(z) paste(z, collapse = "")))

circ <- c(small_circ, big_circ)

length(circ)
