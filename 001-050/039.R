integer_sums <- sapply(1:1000, function(p) {
    bound <- floor(p / 2)
    
    sum(sapply(1:(bound-2), function(a) {
        sum(sapply((a+1):(bound-1), function(b) {
            (p-a-b)^2 == a^2 + b^2
        }))
    }))
})

which(integer_sums == max(integer_sums))
