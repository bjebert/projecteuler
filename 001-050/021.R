source("helpers/primes.R")

d <- function(x) {
    f <- factors(x)
    sum(f[f != x])
}

dvals <- sapply(1:10000, d)

sum(which(sapply(1:10000, function(i) {
    dvals[i] >= 1 && dvals[i] <= 10000 && dvals[i] != i && i == dvals[dvals[i]]
})))

