x <- 100000

while(TRUE) {
    is_permuted_multiple <- length(unique(sapply(strsplit(as.character(sapply(1:6, function(m) m*x)), ""), function(x) {
        paste(sort(x), collapse = "")
    }))) == 1
    
    if(is_permuted_multiple) {
        print(x)
        break
    }
    
    x <- x + 1
}



