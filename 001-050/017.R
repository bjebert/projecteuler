base_map <- setNames(c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
                       "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen",
                       "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety", "one thousand"),
                     as.character(c(1:19, seq(20, 90, 10), 1000)))

num2word <- function(x) {
    if(as.character(x) %in% names(base_map)) {
        return(base_map[[as.character(x)]])
    } else if(x == 0) {
        return("")
    } else if(x >= 21 && x <= 99) {
        return(sprintf("%s-%s", base_map[[as.character(x - x %% 10)]], base_map[[as.character(x %% 10)]]))
    } else if(x >= 100) {
        hundred_base <- sprintf("%s hundred", base_map[[as.character(x %/% 100)]])
        if(x %% 100 == 0) {
            return(hundred_base)
        } else {
            return(sprintf("%s and %s", hundred_base, num2word(x %% 100)))
        }
    }
}

num2len <- function(x) {
    nchar(gsub(" |-", "", num2word(x)))
}

sum(sapply(1:1000, num2len))

