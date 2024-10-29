source("helpers/primes.R")

abundant <- which(sapply(1:28123, function(x) sum(proper_divisors(x)) > x))

is_sum_of_two_abundant <- function(x) {
    possible <- abundant[abundant <= (x / 2)]
    
    if(length(possible) == 0) {
        return(FALSE)
    }
    
    return(any((x - possible) %in% abundant))
}

sum(which(!sapply(1:28213, is_sum_of_two_abundant)))
