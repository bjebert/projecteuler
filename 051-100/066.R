library(Rmpfr)

find_x <- function(D, n = 10) {
    D <- mpfr(D, precBits = 128)
    if(sqrt(D) %% 1 == 0) {
        return(NA)
    }
    
    results <- list()
    
    y <- mpfr(1, precBits = 128)
    while(TRUE) {
        if(sqrt(1 + D * y^2) %% 1 == 0) {
            results[[length(results) + 1]] <- c(sqrt(1 + D * y^2), y)
            
            if(length(results) >= n) {
                return(results)
            }
        }
        y <- y + 1
    }   
}


get_term_sequence <- function(N, len = 100) {
    if(sqrt(N) %% 1 == 0) {
        return(0)
    }
    
    a <- floor(sqrt(N))
    numer <- 1
    b <- -a
    
    for(i in 1:len) {
        denom <- (N - b^2) / numer
        a <- c(a, floor((sqrt(N) + -b) / denom))
        b <- -b - a[length(a)] * denom
        numer <- denom
    }
    
    return(a)
}


get_root_convergent_fraction <- function(N, iter) {
    terms <- get_term_sequence(N)
    
    top <- as.bigz(1)
    btm <- as.bigz(terms[iter])
    
    while(iter > 1) {
        cons <- as.bigz(terms[iter-1])
        tmp <- top
        
        top <- btm
        btm <- cons*btm + tmp
        iter <- iter - 1
    }
    
    return(c(btm, top))
}


get_minimal_solution <- function(N) {
    if(sqrt(N) %% 1 == 0) {
        return(NA)
    }
    
    iter <- 1
    while(TRUE) {
        cfrac <- get_root_convergent_fraction(N, iter = iter)
        if(cfrac[1]^2 - N*cfrac[2]^2 == 1) {
            return(cfrac)
        }
        iter <- iter + 1
    }
}

sol_x <- sapply(sapply(1:1000, get_minimal_solution), function(sol) sol[1])

sol_num <- sapply(sol_x, as.numeric)
which(sol_num == max(sol_num, na.rm = T))
