
x <- matrix(1:25, 5, 5)
colnames(x) <- c("a", "a", "b", "c", "d")
print(x)

bool <- sample(c(TRUE, FALSE), 5, TRUE)
int <- 1:4
chr <- c("a", "a")
cplx <- 1:4 * -1i
tci_bool(bool, nrow(x))
tci_int(int, ncol(x), inv = TRUE)
tci_chr(chr, colnames(x))
tci_im(cplx, nrow(x))

ci_ii(x, 1:10 * -1i)
ci_margin(x, 1:4, 2)
ci_ss(x, n(1:5 * -1i, 1:4), 1:2)

