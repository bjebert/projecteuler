words <- strsplit(gsub('"', '', readLines("inputs/0042_words.txt")), ",")[[1]]

lmap <- setNames(1:26, LETTERS)
word_sums <- sapply(strsplit(words, ""), function(x) sum(lmap[x]))

triangles <- sapply(1:100, function(n) n*(n+1)/2)

sum(word_sums %in% triangles)
