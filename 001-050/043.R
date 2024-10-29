source("helpers/primes.R")

pandigitals0 <- combinat::permn(0:9)

# Can shortcut the first step to speed up computation, and simply check that d4 is even (d2/d3 are irrelevant)
pd_divisible <- pandigitals0[sapply(pandigitals0, function(x) x[4] %% 2 == 0)]

# Check other digit combinations are divisible by next 6 primes
for(i in 3:8) {
    sub_num <- as.numeric(sapply(lapply(pd_divisible, function(x) x[i:(i+2)]), function(x) paste(x, collapse = "")))
    pd_divisible <- pd_divisible[sub_num %% sieve[i-1] == 0]
}

sum(as.numeric(sapply(pd_divisible, function(x) paste(x, collapse = ""))))

