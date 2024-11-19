library(combinat)

cipher <- readLines("inputs/0059_cipher.txt")
cipher_int <- as.numeric(strsplit(cipher, ",")[[1]])

keys <- as.matrix(expand.grid(letters, letters, letters))

s1 <- seq(1, length(cipher_int), 3)
s2 <- seq(2, length(cipher_int), 3)
s3 <- seq(3, length(cipher_int), 3)

get_p_letters <- function(i) {
    key <- rep(as.numeric(sapply(keys[i, ], charToRaw)), times = length(cipher_int) / 3)
    decoded <- sapply(bitwXor(cipher_int, key), function(x) rawToChar(as.raw(x)))
    
    msg <- paste(decoded, collapse = "")
    p <- mean(decoded %in% as.character(c(letters, LETTERS, 0:9, ' ')))
    
    return(list(msg = msg, p = p))
}

p <- setNames(lapply(1:nrow(keys), function(i) get_p_letters(i)), as.character(1:nrow(keys)))
idx <- as.numeric(names(rev(sort(sapply(p, function(x) x[["p"]])))[1:20]))

# key = "exp", found manually by finding first three letters "A", "n", space