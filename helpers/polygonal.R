# Generic form
polygonal <- function(s, n) ((s-2)*n^2 - (s-4)*n)/2

triangle <- function(n) n*(n+1)/2
square <- function(n) n^2
pentagonal <- function(n) n*(3*n-1)/2
hexagonal <- function(n) n*(2*n-1)
heptagonal <- function(n) n*(5*n-3)/2
octagonal <- function(n) n*(3*n-2)