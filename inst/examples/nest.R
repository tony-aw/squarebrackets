obj <- array(1:64, c(4,4,3))
print(obj)
ss_x(obj, n(1:3, 1:2), c(1,3))
# above is equivalent to obj[1:3, , 1:2, drop = FALSE]
