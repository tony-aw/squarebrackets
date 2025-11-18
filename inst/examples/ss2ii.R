
x.dim <- c(10, 10, 3)
x.len <- prod(x.dim)
x <- array(1:x.len, x.dim)
sub <- list(c(4, 3), c(3, 2), c(2, 3))
coord <- ss2coord(sub, x.dim)
print(coord)
ind <- coord2ii(coord, x.dim)
print(ind)
all(x[ind] == c(x[c(4, 3), c(3, 2), c(2, 3)])) # TRUE
coord2 <- ii2coord(ind, x.dim)
print(coord)
all(coord == coord2) # TRUE
sub2 <- coord2ss(coord2)
sapply(1:3, \(i) sub2[[i]] == sub[[i]]) |> all() # TRUE

