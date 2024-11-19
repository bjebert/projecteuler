options(scipen = 999)
library(gmp)

N <- 10000
L <- 5

cubes <- (1:N)^3

cube_table <- lapply(strsplit(as.character(cubes), ""), function(x) sort(table(x)))

for(i in 1:N) {
    if(sum(sapply(cube_table, function(x) identical(cube_table[[i]], x))) == L) {
        print(i^3)
        break
    }
}


