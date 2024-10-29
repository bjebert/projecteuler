frac_str <- paste(1:1e6, collapse = "")
prod(as.numeric(sapply(0:6, function(n) substr(frac_str, 10^n, 10^n))))
