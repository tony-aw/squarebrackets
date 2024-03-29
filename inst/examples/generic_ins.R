
# atomic objects ====

x <- matrix(1:20 , ncol = 4)
print(x)
new <- -1 * x
sb_before(x, new, 1)
sb_after(x, new, 1)
sb_before(x, new, 2)
sb_after(x, new, 2)


#############################################################################


# factors ====

x <- factor(letters)
new <- factor("foo")
sb_before(x, new)
sb_after(x, new)


#############################################################################


# lists ====

x <- as.list(1:5)
new <- lapply(x, \(x)x*-1)
print(x)
sb2_before(x, new)
sb2_after(x, new)


#############################################################################


# recursive arrays / dimensional lists ====

x <- matrix(c(as.list(1:20), as.list(letters[1:20])) , ncol = 8) |> t()
dimnames(x) <- list(letters[1:8], letters[1:5])
print(x)
new <- matrix(c(as.list(-1:-20), as.list(letters[26:7])) , ncol = 8) |> t()
sb2_before(x, new, 1)
sb2_after(x, new, 1)
sb2_before(x, new, 2)
sb2_after(x, new, 2)



#############################################################################


# data.frame-like objects ====

x <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
new <- data.frame(e = 101:110)
sb2_before(x, new, 2)
sb2_after(x, new, 2)
new <- x[1,]
sb2_before(x, new, 1)
sb2_after(x, new, 1)

