triangle_text <- readLines("inputs/0067_triangle.txt")

triangle <- lapply(lapply(triangle_text, function(x) strsplit(x, " ")[[1]]), as.numeric)
max_possible <- c(triangle[[1]], lapply(2:length(triangle), function(i) rep(0, i)))

for(i in 2:length(max_possible)) {
    for(j in 1:i) {
        v <- triangle[[i]][j]
        
        if(j == 1) {
            max_possible[[i]][j] <- v + max_possible[[i-1]][1]
        } else if(j == i) {
            max_possible[[i]][j] <- v + max_possible[[i-1]][i-1]
        } else {
            max_possible[[i]][j] <- v + max(max_possible[[i-1]][j-1], max_possible[[i-1]][j])
        }        
    }
}

max(max_possible[[length(max_possible)]])


