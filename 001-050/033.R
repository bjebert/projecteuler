library(gmp)

is_curious <- function(a, b) {
    target <- gmp::as.bigq(a, b)
    
    ac <- strsplit(as.character(a), "")[[1]]
    bc <- strsplit(as.character(b), "")[[1]]
    
    w <- sapply(ac, function(x) x %in% bc)
    
    if(!any(w)) {
        return(FALSE)
    }
    
    for(digit in ac[w]) {
        if(digit == "0") {
            next
        }
        # Try removing the digit from bc, and see if fraction remains the same
        
        # Make sure to check all possible positions to remove digit from!
        w_a <- which(ac == digit)
        w_b <- which(bc == digit)
        
        for(i_a in w_a) {
            for(i_b in w_b) {
                new_a <- as.numeric(paste(ac[setdiff(1:2, i_a)], collapse = "")) 
                new_b <- as.numeric(paste(bc[setdiff(1:2, i_b)], collapse = ""))
                
                if(new_b == 0) {
                    return(FALSE)
                }
                
                if(gmp::as.bigq(new_a, new_b) == target) {
                    return(TRUE)
                }
            }
        }
    }
    
    return(FALSE)
}

cur <- list()

for(a in 10:98) {
    for(b in (a+1):99) {
        if(is_curious(a, b)) {
            cur[[length(cur) + 1]] <- c(a, b)
        }        
    }
}

prod <- Reduce(`*`, sapply(cur, function(x) x[1] / x[2]))  #  = 1/100
