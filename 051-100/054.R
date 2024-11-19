poker <- readLines("inputs/0054_poker.txt")

hand1 <- lapply(strsplit(poker, " "), function(x) x[1:5])
hand2 <- lapply(strsplit(poker, " "), function(x) x[6:10])

rank_map <- c(setNames(2:9, as.character(2:9)), `T` = 10, `J` = 11, `Q` = 12, `K` = 13, `A` = 14)

strength <- function(hand) {
    # return (strength, tiebreaker)
    # removed possibilities of A,2,3,4,5 straights
    
    rank <- substr(hand, 1, 1)
    suit <- substr(hand, 2, 2)
    num <- as.numeric(sort(rank_map[rank]))
    tab <- table(num)
    
    # royal flush
    if(all.equal(num, 10:14) == TRUE) {
        return(10)
    }
    
    # straight flush
    if(length(unique(suit)) == 1 && all.equal(num, num[1]:(num[1]+4)) == TRUE) {
        return(c(9, num[4]))
    }
    
    # four of a kind
    if(any(tab == 4)) {
        return(c(8, as.numeric(names(tab)[tab == 4])))
    }
    
    # full house
    if(any(tab == 3) && any(tab == 2)) {
        return(c(7, as.numeric(names(tab)[tab == 3]), as.numeric(names(tab)[tab == 2])))
    }
        
    # flush
    if(length(unique(suit)) == 1) {
        return(c(6, max(num)))
    }
    
    # straight
    if(all.equal(num, num[1]:(num[1]+4)) == TRUE) {
        return(c(5, num[4]))
    }
    
    # three of a kind
    if(any(tab == 3)) {
        r3 <- as.numeric(names(tab)[tab == 3])
        others <- num[num != r3]
        
        return(c(4, as.numeric(names(tab)[tab == 3]), others[2:1]))
    }
    
    # two pair
    if(sum(tab == 2) == 2) {
        p2 <- as.numeric(names(tab)[tab == 2])
        other <- setdiff(num, p2)
        
        return(c(3, max(p2), min(p2), other))
    }
    
    # pair
    if(sum(tab == 2) == 1) {
        p <- as.numeric(names(tab)[tab == 2])
        others <- setdiff(num, p)
        
        return(c(2, p, others[3:1]))
    }
    
    # high card
    return(c(1, num[5:1]))
}


winner <- function(h1, h2) {
    str1 <- strength(h1)  
    str2 <- strength(h2)

    for(j in 1:min(length(str1), length(str2))) {
        if(str1[j] > str2[j]) {
            return(1)
        } else if(str2[j] > str1[j]) {
            return(2)
        }
    }
}


win <- sapply(1:1000, function(i) winner(hand1[[i]], hand2[[i]]))

sum(win == 1)
