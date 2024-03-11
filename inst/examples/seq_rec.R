
seq_rec() # by default gives Fibonacci numbers
seq_rec(0:3, 10L, sum) # a weird shifted version of Fibonacci
seq_rec(inits = 2:1) # Lucas numbers
c(1, seq_rec(c(1, 2), f = prod)) # Multiplicative Fibonacci
seq_rec(f = \(x) 2 * x[2] + x[1]) # Pell numbers
seq_rec(inits = c(1, 0), f = \(x) 2 * x[1]) # see https://oeis.org/A077957
seq_rec(f = \(x) x[2] + 2 * x[1]) # Jacobsthal numbers
seq_rec(c(1, 1, 1), f = \(x) x[1] + x[2]) # Padovan sequence
seq_rec(c(3, 0, 2), f = \(x) x[1] + x[2]) # Perrin numbers
seq_rec(c(0, 1, 3), f = \(x) 3 * x[3] - 3 * x[2] + x[1]) # Triangular numbers

