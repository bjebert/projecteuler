# UPDATE: In hindsight, it's actually faster brute forcing without caching/dynamic programming.  Oops!
# Kind of slow, but runs within ~5 minutes. Use of cache needs optimising.

options(scipen=99)

library(hash)
cache <- hash(1, 1)


collatz <- function(x, cache) {
    i <- x[1]
    
    if(has.key(as.character(i), cache)) {
        cache[as.character(x)] <- cache[[as.character(i)]]:(cache[[as.character(i)]] + length(x) - 1)
        return(cache)
    }
    else {
        if(i %% 2 == 0) {
            return(collatz(c(i/2, x), cache))
        } else {
            return(collatz(c(3*i + 1, x), cache))
        }
    }
}


i <- 1e6

while(TRUE) {
    if(!(has.key(as.character(i), cache))) {
        cache <- collatz(i, cache)
    }
    
    if(i %% 100 == 0) {
        print(i)
    }
    
    i <- i - 1
}

v <- values(cache)
names(which(v == max(v)))
