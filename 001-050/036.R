options(scipen=99)

to_binary <- function(num) {
    
    bits <- rev(as.integer(intToBits(num)))
    w <- which(bits == 1)
    
    if(length(w) == 0) {
        return(paste(bits, collapse = ""))
    } else {
        return(paste(bits[w[1]:length(bits)], collapse = ""))
    }
}

nums_split <- strsplit(as.character(1:1e6), "")
binary_split <- strsplit(sapply(1:1e6, to_binary), "")

nums_rev <- lapply(nums_split, rev)
binary_rev <- lapply(binary_split, rev)

w <- sapply(1:1e6, function(i) {
    identical(nums_split[[i]], nums_rev[[i]]) && identical(binary_split[[i]], binary_rev[[i]])
})

sum((1:1e6)[w])

