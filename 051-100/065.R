# 065: Convergents of e

library(gmp)

# e = [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1, ..., 1, 2k, 1, ...]

2                                                          # 2
2 + 1/1                                                    # 3
2 + 1/(1 + 1/2)                                            # 8/3
2 + 1/(1 + 1/(2 + 1/1))                                    # 11/4
2 + 1/(1 + 1/(2 + 1/(1 + 1/1)))                            # 19/7
2 + 1/(1 + 1/(2 + 1/(1 + 1/(1 + 1/4))))                    # 87/32
2 + 1/(1 + 1/(2 + 1/(1 + 1/(1 + 1/(4 + 1/1)))))            # 106/39
2 + 1/(1 + 1/(2 + 1/(1 + 1/(1 + 1/(4 + 1/(1 + 1/1))))))    # 193/71


e_term <- function(N) {
    if(N == 0) {
        return(2)
    }
    
    if(N %% 3 == 2) 2 * ((N %/% 3) + 1) else 1
}


convergent <- function(N) {
    top <- as.bigz(1)
    btm <- as.bigz(e_term(N-1))
    
    while(N > 1) {
        cons <- as.bigz(e_term(N-2))
        tmp <- top
        
        top <- btm
        btm <- cons*btm + tmp
        N <- N - 1
    }
    
    return(c(btm, top))
}


sapply(1:10, convergent)

sum(as.numeric(strsplit(as.character(convergent(100)[1]), "")[[1]]))


