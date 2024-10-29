for(n in 2:5) {
    raw <- sapply(1:49999, function(x) {
        paste(as.character(x*1:n), collapse = "")
    })
    
    s <- strsplit(raw, "")
    
    s_filt <- s[sapply(s, function(x) length(x) == 9 & length(unique(x)) == 9)]
    
    if(length(s_filt) > 0) {
        s_filt <- s_filt[sapply(s_filt, function(x) !("0" %in% x))]
        print(max(as.numeric(sapply(s_filt, function(x) paste(x, collapse = "")))))
    }
}


