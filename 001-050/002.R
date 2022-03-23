fib <- c(1, 2)

while(fib[length(fib)] < 4000000) {
    N <- length(fib)
    fib <- c(fib, fib[N] + fib[N-1])    
}

sum(fib[fib %% 2 == 0])
