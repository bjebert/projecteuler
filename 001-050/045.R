triangles <- NULL
pentagonals <- NULL
hexagonals <- NULL

solved <- FALSE
N <- 1000

while(!solved) {
    triangles <- c(triangles, sapply((N-999):N, function(n) n*(n+1)/2))
    pentagonals <- c(pentagonals, sapply((N-999):N, function(n) n*(3*n-1)/2))
    hexagonals <- c(hexagonals, sapply((N-999):N, function(n) n*(2*n-1)))
    
    w <- which(sapply(triangles[(N-999):N], function(x) x %in% pentagonals && x %in% hexagonals))
    
    if(length(w) > 0 && max(w) > 285) {
        print(w)
        print(triangles[(N-999):N][w])
        solved <- TRUE
    } else {
        N <- N + 1000
        print(N)
    }
}
