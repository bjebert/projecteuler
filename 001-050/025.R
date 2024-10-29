library(gmp)

digits <- function(x) {
    nchar(as.character(x))
}

f <- c(as.bigz(1), as.bigz(1))
N <- length(f)

while(digits(f[N]) < 1000) {
    f <- c(f, f[N-1] + f[N])
    N <- length(f)
}

N
