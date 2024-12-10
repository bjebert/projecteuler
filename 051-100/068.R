source("helpers/polygonal.R")

# Some simple observations:
# a) 10 must be on the outer to create a 16-digit string
# b) Total sum = 2*sum(Inner) + sum(Outer) (which must be divisible by 5)
# c) Inner sum = sum(1:10) - sum(Outer)

# thus, can simplify to filter outer sum %% 5 == 0.

# Select combinations of numbers to appear in the outer, filter out those that don't work

outer_combos <- cbind(gtools::permutations(n = 9, r = 4), 10)
total_sums <- 2 * (sum(1:10) - rowSums(outer_combos)) + rowSums(outer_combos)
valid_outer_combos <- outer_combos[total_sums %% 5 == 0, ]



# N = 3

# outer_combos <- gtools::permutations(n = 6, r = 3)
# total_sums <- 2 * (sum(1:6) - rowSums(outer_combos)) + rowSums(outer_combos)
# valid_outer_combos <- outer_combos[total_sums %% 3 == 0, ]


get_rings <- function(outer_combo) {
    N <- length(outer_combo)
    
    inner_set <- setdiff(1:(N*2), outer_combo)
    target_sum <- (2 * (sum(1:(N*2)) - sum(outer_combo)) + sum(outer_combo)) / N
    
    # Each 3-ring must add up to target_sum
    inner_mat <- matrix(unlist(combinat::permn(inner_set)), ncol = N, byrow = T)
    
    w <- which(sapply(1:nrow(inner_mat), function(i) {
        inner_combo <- inner_mat[i, ]
        
        all(sapply(1:N, function(j) {
            outer_combo[j] + inner_combo[(j-1) %% N + 1] + inner_combo[j %% N + 1] == target_sum
        }))
    }))
    
    if(length(w) == 1) {
        inner_combo <- inner_mat[w, ]
        
        groups <- sapply(1:N, function(j) {
            sprintf("%s%s%s", outer_combo[j], inner_combo[(j-1) %% N + 1], inner_combo[j %% N + 1])
        })
        
        groups <- gsub("10", "T", groups)
        
        # Reshuffle groups in a circular manner s.t. lowest group is first
        wg <- which(groups == min(groups))
        groups_clockwise <- groups[(1:N + wg - 2) %% N + 1]
        
        print(paste(groups_clockwise, collapse = ""))
    }
}

rings <- sapply(1:nrow(valid_outer_combos), function(i) {
    outer_combo <- valid_outer_combos[i,]
    get_rings(outer_combo)
})

gsub("T", "10", rev(sort(unique(unlist(rings[!sapply(rings, is.null)]))))[1])

