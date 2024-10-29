# A slightly faster, vectorised solution (though brute-forcing is still probably faster < 1e6)

nums <- 1:1e6
lens <- rep(1, 1e6)
w <- nums != 1

while(sum(w) > 0) {
    print(sum(w))
    
    w_even <- w & nums %% 2 == 0
    w_odd <- w & nums %% 2 != 0
    
    nums[w_even] <- nums[w_even] / 2
    nums[w_odd] <- nums[w_odd] * 3 + 1
    
    lens[w] <- lens[w] + 1
    
    w <- nums != 1
    
    if(sum(w) >= 100) {
        # --- Dynamic - if num is at a number which has finished sequence, set it to 1 and add length of existing
    
        n <- w & nums <= 1e6
        cvg <- nums[nums[n]] == 1
        
        lens[n][cvg] <- lens[n][cvg] + lens[nums[n][cvg]] - 1
        nums[n][cvg] <- 1
            
        w <- nums != 1
    }
}


which(lens == max(lens))

