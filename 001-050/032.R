source("helpers/primes.R")

# Product is 4 digits max (at 10000, sqrt(10000) = 100 which is 3 digits, 3 + 2 + 5 > 9)
is_pandigital_product <- function(p) {
    f <- factors(p)
    
    if(length(f) == 2) {
        return(FALSE)
    }
    
    any(sapply(2:(length(f)/2), function(i) {
        x1 <- f[i]
        x2 <- p / f[i]
        
        nums <- unique(unlist(strsplit(as.character(c(x1, x2, p)), "")))
        
        return(length(unique(nums)) == 9 & all(nums != "0"))
    }))
}


domain <- 101:9999
filt <- domain[sapply(strsplit(as.character(domain), ""), function(x) all(x != "0") & length(x) == length(unique(x)))]

w <- sapply(filt, is_pandigital_product)

sum(filt[w])





