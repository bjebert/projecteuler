library(gmp)
sum(as.numeric(strsplit(as.character(factorial(as.bigz(100))), "")[[1]]))
