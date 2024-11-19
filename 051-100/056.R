max_sum <- 0

for(a in 1:100) {
    for(b in 1:100) {
        x <- as.bigz(a) ^ as.bigz(b)
        digit_sum <- sum(as.numeric(strsplit(as.character(x), "")[[1]]))
        
        if(digit_sum > max_sum) {
            max_sum <- digit_sum
            print(digit_sum)
        }
    }
}

