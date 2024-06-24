
# atomic arrays ====
x <- matrix(1:12,3,4)
dimnames(x) <- n(letters[1:3], LETTERS[1:4])
names(x) <- month.abb
print(x)
y <- x+100
arg.list <- list(x = x, y=y)
bind_array(arg.list, along=0, name_flat = TRUE) # binds on new dimension before first
bind_array(arg.list, along=1, name_flat = TRUE) # binds on first dimension
bind_array(arg.list, along=2, name_flat = TRUE)
bind_array(arg.list, along=3, name_flat = TRUE) # bind on new dimension after last


################################################################################


# recursiv arrays ====
x <- matrix(as.list(1:12),3,4)
dimnames(x) <- n(letters[1:3], LETTERS[1:4])
names(x) <- month.abb
print(x)
y <- lapply(x, \(x) + 100)
dim(y) <- dim(x)
arg.list <- list(x = x, y=y)
bind2_array(arg.list, along=0, name_flat = TRUE) # binds on new dimension before first
bind2_array(arg.list, along=1, name_flat = TRUE) # binds on first dimension
bind2_array(arg.list, along=2, name_flat = TRUE)
bind2_array(arg.list, along=3, name_flat = TRUE) # bind on new dimension after last

