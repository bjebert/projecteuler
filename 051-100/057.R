library(gmp)

numers <- gmp::as.bigz(c(1, 3))
denoms <- gmp::as.bigz(c(1, 2))

for(i in 3:1000) {
    numers <- c(numers, gmp::as.bigz(2*numers[i-1] + numers[i-2]))
    denoms <- c(denoms, gmp::as.bigz(2*denoms[i-1] + denoms[i-2]))
}

numlen <- sapply(numers, function(x) ceiling(log(x+1, base = 10)))
denlen <- sapply(denoms, function(x) ceiling(log(x+1, base = 10)))

sum(numlen > denlen)
