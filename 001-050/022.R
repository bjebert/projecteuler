names <- readLines("inputs/0022_names.txt")

names_ord <- sort(gsub('"', '', strsplit(names, ",")[[1]]))
names_split <- strsplit(names_ord, "")

lmap <- setNames(1:26, LETTERS)

sum(sapply(1:length(names_split), function(i) {
    i * sum(lmap[names_split[[i]]])
}))
