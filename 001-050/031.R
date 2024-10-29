coins <- c(1, 2, 5, 10, 20, 50, 100, 200)

ways <- function(num, coins) {
    coins <- coins[coins <= num]
    
    if(length(coins) <= 1) {
        return(1)
    } else {
        w <- sapply(1:length(coins), function(i) ways(num - coins[i], coins[1:i]))
        
        return(sum(w))
    }
}

ways(200, coins)

