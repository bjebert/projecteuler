source("helpers/primes.R")

# So ugly & slow.  And, wouldn't work if prime involved was > 1e4 (would need more calculation).
# Better ways to do it involve a recursive search, and likely a lazy evaluation for compatibility, rather
# than pre-calculating (which takes a long time).

# Nicer solution implemented in 060-2.

# Start with a smaller set of primes
filt <- sieve[sieve < 1e4 & !(sieve %in% c(2, 5))]

# For each element in sieve, find other elements that would be incompatible with being concatenated.  Track in a boolean array
compatible <- setNames(lapply(1:length(filt), function(x) rep(FALSE, length(filt))), filt)

concat_mat <- sapply(1:(length(filt)-1), function(i) {
    t(sapply((i+1):length(filt), function(j) {
        as.numeric(c(paste0(filt[i], filt[j]), paste0(filt[j], filt[i])))
    }))
})


for(i in 1:length(concat_mat)) {
    for(j in 1:nrow(concat_mat[[i]])) {
        is_compatible <- all(sapply(concat_mat[[i]][j,], is_prime))
        
        compatible[[i]][j+i] <- is_compatible
        compatible[[j+i]][i] <- is_compatible
    }
    
    compatible[[i]][i] <- FALSE
}


lowest <- Inf

for(i in filt) {
    match1 <- filt[compatible[[as.character(i)]]]
    
    for(j in match1) {
        match2 <- intersect(match1, filt[compatible[[as.character(j)]]])
        
        if(length(match2) > 1) {
            for(k in match2) {
                match3 <- intersect(match2, filt[compatible[[as.character(k)]]])
                
                if(length(match3) > 1) {
                    for(x in match3) {
                        match4 <- intersect(match3, filt[compatible[[as.character(x)]]])
                        
                        for(y in match4) {
                            match_sum <- i+j+k+x+y
                            
                            if(match_sum < lowest) {
                                lowest <- match_sum
                                print(lowest)
                                print(sprintf("%d/%d/%d/%d/%d", i, j, k, x, y))
                            }
                        }    
                    }
                }
                
                        
            }
        }
    }
}




