source("helpers/primes.R")


tris <- c(1)

while(TRUE) {
    N <- length(tris)
    tris <- c(tris, tris[N] + N + 1)
    
    num <- tris[N+1]
    
    if(length(factors(num)) > 500) {
        print(num)
        break
    }
}
