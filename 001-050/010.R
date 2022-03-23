source("helpers/primes.R")

primes <- copy(sieve)

for(i in (max(primes)+1):1999999) {  # Zzz... slow R
    if(is_prime_sieve(i)) {
        primes <- c(primes, i)
    }
}

print(sum(primes))
