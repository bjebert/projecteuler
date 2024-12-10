get_period_length <- function(s) {
    if(length(s) == 1) {
        return(0)
    }
    
    for(len in 1:floor(length(s)/2)) {
        lu <- unique(lapply(seq(1, length(s), len), function(i) {
            hi <- i+len-1
            
            if(hi <= length(s)) {
                return(s[i:(i+len-1)])
            }
        }))
        
        lu <- lu[!sapply(lu, is.null)]
        if(length(lu) == 1) {
            return(len)
        }
    }
    return(NA)
}


get_sequence <- function(N, len = 100) {
    if(sqrt(N) %% 1 == 0) {
        return(0)
    }
    
    a <- floor(sqrt(N))
    numer <- 1
    b <- -a
    
    for(i in 1:len) {
        denom <- (N - b^2) / numer
        a <- c(a, floor((sqrt(N) + -b) / denom))
        b <- -b - a[length(a)] * denom
        numer <- denom
    }
    
    return(a[2:length(a)])
}

test_len <- 500
period_lengths <- NA

while(any(is.na(period_lengths))) {
    print(test_len)
    period_lengths <- sapply(2:10000, function(N) get_period_length(get_sequence(N, len = test_len)))
    test_len <- test_len * 2
}

sum(period_lengths %% 2 == 1)

