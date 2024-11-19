count <- 0

for(n in 23:100) {
    
    was_last_over <- FALSE
    for(r in 1:100) {
        ways <- choose(n, r)
        
        if(ways >= 1e6) {
            was_last_over <- TRUE
            count <- count + 1
                        
        } else if(ways < 1e6 && was_last_over) {
            break
        }
    }
}

print(count)
