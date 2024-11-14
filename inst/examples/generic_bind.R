
# atomic arrays ====
x <- matrix(1:12,3,4)
dimnames(x) <- n(letters[1:3], LETTERS[1:4])
names(x) <- month.abb
print(x)
y <- x+100
arg.list <- list(x = x, y = y)
bind_array(arg.list, along=0) # binds on new dimension before first
bind_array(arg.list, along=1) # binds on first dimension
bind_array(arg.list, along=2)
bind_array(arg.list, along=3) # bind on new dimension after last


################################################################################


# recursive arrays ====
x <- matrix(as.list(1:12),3,4)
dimnames(x) <- n(letters[1:3], LETTERS[1:4])
names(x) <- month.abb
print(x)
y <- lapply(x, \(x) + 100)
dim(y) <- dim(x)
arg.list <- list(x = x, y=y)
bind2_array(arg.list, along=0) # binds on new dimension before first
bind2_array(arg.list, along=1) # binds on first dimension
bind2_array(arg.list, along=2)
bind2_array(arg.list, along=3) # bind on new dimension after last

