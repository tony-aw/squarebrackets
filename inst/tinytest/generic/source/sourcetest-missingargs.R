
x <- 1:10
temp.fun(x)

if(test_use_factors) {
  x <- factor(letters)
  temp.fun(x)
}

x <- matrix(1:20, ncol = 4)
temp.fun(x)

x <- array(1:27, c(3,3,3))
temp.fun(x)

enumerate <- enumerate + 4
