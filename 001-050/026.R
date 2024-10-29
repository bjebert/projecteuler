# Takes way too long (~20 minutes)

library(Rmpfr)
precision <- 10000


get_cycle_length <- function(x) {
    # Require minimum 2 repeats
    digits <- strsplit(x, "")[[1]]
    
    for(n in 1:floor(length(digits) / 2)) {
        s <- seq(n, length(digits), n)
        
        if(length(unique(sapply(s, function(i) paste(digits[(i+1-n):i], collapse = "")))) == 1) {
            return(n)
        }
    }
    
    return(1)
}


lens <- sapply(2:1000, function(d) {
    print(d)
    
    num <- mpfr(1, precBits = precision) / mpfr(d, precBits = precision)
    x <- gsub(" |NaN", "", strsplit(capture.output(str(num, give.head = FALSE, digits.d = precision)), "\\.")[[1]][2])
    get_cycle_length(x)
})

(2:1000)[which(lens == max(lens))]




