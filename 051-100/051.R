source("helpers/primes.R")
sieve <- readRDS("helpers/prime_sieve.rds")

# add 6-digits to sieve
rng <- 100000:999999
primes6 <- rng[sapply(rng, function(x) is_prime_sieve(x, sieve))]
sieve <- unique(c(sieve, primes6))

# 2-digit primes ----------------------------------------------------------

primes2 <- sieve[ceiling(log(sieve + 1, base = 10)) == 2]

mat2 <- t(sapply(strsplit(as.character(primes2), ""), as.numeric))

# Asterisk on idx 1:
unique(mat2[,2])
# *1, *3, *7, *9

sum(mat2[,2] == 1)
sum(mat2[,2] == 3)
sum(mat2[,2] == 7)
sum(mat2[,2] == 9)

# Asterisk on idx 2:
unique(mat2[,1])
# 1*, 2*, 3*, 4*, 5*, 6*, 7*, 8*, 9*

sum(mat2[,1] == 1)
sum(mat2[,1] == 2)
sum(mat2[,1] == 3)
sum(mat2[,1] == 4)
sum(mat2[,1] == 5)
sum(mat2[,1] == 6)
sum(mat2[,1] == 7)
sum(mat2[,1] == 8)
sum(mat2[,1] == 9)

# N-digit primes ----------------------------------------------------------

N <- 6

primesN <- sieve[ceiling(log(sieve + 1, base = 10)) == N]
matN <- t(sapply(strsplit(as.character(primesN), ""), as.numeric))

x_idx <- unlist(sapply(1:(N-1), function(i) combn(1:N, i, simplify = F)), recursive = F)

for(k in x_idx) {
    mi <- setdiff(1:N, k)
    
    mfilt <- matN[sapply(1:nrow(matN), function(i) length(unique(matN[i, k])) == 1), ]
    
    m <- mfilt[, mi]    
    t <- table(sapply(1:nrow(m), function(x) paste(m[x,], collapse = "")))
    
    if(any(t[t >= 8])) {
        print(k)  # asterisk indices
        print(t[t >= 8])  # other values
    }
}

sapply(sapply(1:9, function(i) as.numeric(sprintf("%d2%d3%d3", i, i, i))), is_prime)
