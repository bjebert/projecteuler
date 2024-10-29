N <- 1001
corners <- list(1, c(3, 5, 7, 9))

# Each additional spiral, each corner val will increase by +8 from its last increase
for(i in 3:ceiling(N / 2)) {
    corners[[i]] <- corners[[i-1]] + c(2, 4, 6, 8) + (i-2) * 8
}

sum(unlist(corners))
