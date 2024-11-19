source("helpers/polygonal.R")

N <- 500
figurate <- list(triangle(1:N),
                 square(1:N),
                 pentagonal(1:N),
                 hexagonal(1:N),
                 heptagonal(1:N),
                 octagonal(1:N))

fig4 <- setNames(lapply(figurate, function(x) {
    x[ceiling(log(x + 1, base = 10)) == 4]
}), 1:6)

L <- length(fig4)

# Need to find a length-6 cyclic set; will use DFS, starting with set 1
Q <- lapply(fig4[[1]], function(x) list(x = x, v = "1"))

while(length(Q) >= 1) {
    curr <- Q[[1]]
    Q <- Q[-1]
    
    if(length(curr[["x"]]) == L) {
        if(curr[["x"]][L] %% 100 == curr[["x"]][1] %/% 100) {
            ans <- curr
            print(ans)
            print(sum(ans[["x"]]))
            break
        } else {
            next
        }
    }
    
    # Find neighbours
    potential <- fig4[-which(names(fig4) %in% curr[["v"]])]
    neighbours <- lapply(potential, function(x) x[x %/% 100 == curr[["x"]][length(curr[["x"]])] %% 100])
    
    # Add neighbours to queue
    if(length(neighbours) == 0) {
        next
    }
    
    neighbours_flat <- unlist(lapply(1:length(neighbours), function(i) {
        lapply(neighbours[[i]], function(x) list(x = c(curr[["x"]], x), v = c(curr[["v"]], names(neighbours)[i])))
    }), recursive = F)
    
    Q <- c(neighbours_flat, Q)
}
