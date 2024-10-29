library(gmp)

res <- lapply(2:100, function(a) {
    sapply(2:100, function(b) {
        as.character(as.bigz(a) ^ as.bigz(b))
    })
})

length(unique(unlist(res)))
