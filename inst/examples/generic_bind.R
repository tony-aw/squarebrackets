
# atomic arrays ====
x <- matrix(1:12,3,4)
y <- x+100
arg.list <- list(x = x, y=y)
bind_array(arg.list, along=0) # binds on new dimension before first
bind_array(arg.list, along=1) # binds on first dimension
bind_array(arg.list, along=2)
bind_array(arg.list, along=3) # bind on new dimension after last


################################################################################


# recursiv arrays ====
x <- matrix(as.list(1:12),3,4)
y <- lapply(x, \(x) + 100)
dim(y) <- dim(x)
arg.list <- list(x = x, y=y)
bind2_array(arg.list, along=0) # binds on new dimension before first
bind2_array(arg.list, along=1) # binds on first dimension
bind2_array(arg.list, along=2)
bind2_array(arg.list, along=3) # bind on new dimension after last

