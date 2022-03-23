source("helpers/primes.R")

# Start by finding smallest number that is divisible by 20 & 19 and iterate down to 10
# This allows us to increase increment by that number at each step (much faster)

nums <- c(20, 19)
curr <- 20
inc <- 20

while(!(10 %in% nums)) {
    while(TRUE) {
        if(all(curr %% nums == 0)) {
            break
        }
        curr <- curr + inc
    }
    inc <- curr
    nums <- c(nums, min(nums) - 1)
}

curr
