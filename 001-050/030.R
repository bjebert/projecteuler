upper_bound <- 6 * 9^5

w <- sapply(11:upper_bound, function(i) {
    sum(as.numeric(strsplit(as.character(i), "")[[1]])^5) == i
})

sum((11:upper_bound)[w])
