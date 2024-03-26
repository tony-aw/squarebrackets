
seq_rec2() # by default gives Fibonacci numbers
seq_rec2(inits = 2:1) # Lucas numbers
c(1, seq_rec2(c(1, 2), inop = "*")) # Multiplicative Fibonacci
seq_rec2(m = c(2L, 1L)) # Pell numbers
seq_rec2(inits = c(1, 0), m = c(0L, 2L)) # see https://oeis.org/A077957
seq_rec2(m = c(1L, 2L)) # Jacobsthal numbers

