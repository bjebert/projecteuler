N <- 1000

while(TRUE) {
    P <- sapply(1:N, function(n) n*(3*n-1)/2)
    
    res <- sapply(1:(length(P)-1), function(j) {
        sapply((j+1):length(P), function(k) {
            if((P[j] + P[k]) %in% P && (P[k] - P[j]) %in% P) {
                print(abs(P[k] - P[j]))    
                return(TRUE)
            }
            
            return(FALSE)
        })
    })
    
    if(any(unlist(res, recursive = T))) {
        break
    }
    
    N <- N + 1000
    print(N)
}


