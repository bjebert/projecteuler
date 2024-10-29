upper <- factorial(9) * 7
range <- 3:upper

vals <- sapply(strsplit(as.character(range), ""), as.numeric)
fact_sums <- sapply(vals, function(x) sum(sapply(x, factorial)))
sum(range[fact_sums == range])
