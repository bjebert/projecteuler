library(Rmpfr)

s <- Reduce(`+`, sapply(1:1000, function(x) {
    as.bigz(x) ^ as.bigz(x)
}))

paste(tail(strsplit(as.character(s), "")[[1]], 10), collapse = "")
