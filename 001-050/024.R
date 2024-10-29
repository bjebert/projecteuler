possible <- 0:9
target <- 1e6
digits <- NULL

while(target > 0 && length(possible) > 0) {
    i <- ((target - 1) %/% factorial(length(possible) - 1))
    
    digits <- c(digits, possible[i + 1])
    target <- target - factorial(length(possible) - 1) * i
    possible <- possible[possible != digits[length(digits)]]
}

paste(c(digits, possible), collapse = "")