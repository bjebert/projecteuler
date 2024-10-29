# 1x1 grid
paths <- c(1, 2)

for(n in 2:20) {
    paths <- c(cumsum(paths), sum(paths) * 2)
    print(tail(paths, 1))
}
