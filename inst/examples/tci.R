
x <- matrix(1:25, 5, 5)
colnames(x) <- c("a", "a", "b", "c", "d")
print(x)

bool <- sample(c(TRUE, FALSE), 5, TRUE)
int <- 1:4
chr <- c("a", "a")
tci_bool(bool, nrow(x))
tci_int(int, ncol(x), -1)
tci_chr(chr, colnames(x))

ci_ii(x, 1:10)
ci_margin(x, 1:4, 2)
ci_ss(x, n(~ .bi(-1:-5), 1:4), 1:2)

