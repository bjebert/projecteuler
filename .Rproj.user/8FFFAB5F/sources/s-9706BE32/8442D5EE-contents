palindromes <- c()
for(a in 999:100) {
    for(b in 999:100) {
        char_vec <- strsplit(as.character(a*b), "")[[1]]
        if(all(char_vec == rev(char_vec))) {
            palindromes <- c(palindromes, a*b)
        }
    }
}

print(max(palindromes))
