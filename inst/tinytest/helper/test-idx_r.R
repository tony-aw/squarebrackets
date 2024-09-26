
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0

# argument i ====
x <- 1:10
slice_start <- c(
  1, 2, -1, -2
) * 1i
seq_start <- c(
  1, 2, length(x), length(x) - 1
)
slice_end <- c(
  1, 2, -1, -2
) * 1i
seq_end <- c(
  1, 2, length(x), length(x) - 1
)
slice_by <- c(
  1, -1, 2, -2
)

for(i in seq_along(slice_start)) {
  for(j in seq_along(slice_end)) {
    for(k in seq_along(slice_by)) {
      seq_by <- ifelse(seq_start[i] > seq_end[j], -1, 1) * abs(slice_by[k])
      
      expect_equal(
        idx_r(x, 0, slice_start[i], slice_end[j], slice_by[k]),
        seq(seq_start[i], seq_end[j], seq_by)
      ) |> errorfun()
      
      enumerate <- enumerate + 1L
      
    }
  }
}


for(i in seq_along(slice_by)) {
  if(slice_by[i] > 0) {
    expect_equal(
      idx_r(x, 0, by = slice_by[i]),
      seq(1, length(x), slice_by[i])
    ) |> errorfun()
  }
  else {
    expect_equal(
      idx_r(x, 0, by = slice_by[i]),
      seq(length(x), 1, slice_by[i])
    ) |> errorfun()
  }
  
  enumerate <- enumerate + 1L
  
}

################################################################################
# single array arguments ====

x <- matrix(1:50, ncol = 10)
slice_start <- c(
  1, 2, -1, -2
) * 1i
seq_start <- c(
  1, 2, ncol(x), ncol(x) - 1
)
slice_end <- c(
  1, 2, -1, -2
) * 1i
seq_end <- c(
  1, 2, ncol(x), ncol(x) - 1
)
slice_by <- c(
  1, -1, 2, -2
)

for(i in seq_along(slice_start)) {
  for(j in seq_along(slice_end)) {
    for(k in seq_along(slice_by)) {
      seq_by <- ifelse(seq_start[i] > seq_end[j], -1, 1) * abs(slice_by[k])
    
      
      expect_equal(
        idx_r(x, 2, slice_start[i], slice_end[j], slice_by[k]),
        seq(seq_start[i], seq_end[j], seq_by)
      ) |> errorfun()
      
      enumerate <- enumerate + 1L
      
    }
  }
}

for(i in seq_along(slice_by)) {
  if(slice_by[i] > 0) {
    expect_equal(
      idx_r(x, 2, by = slice_by[i]),
      seq(1, ncol(x), slice_by[i])
    ) |> errorfun()
  }
  else {
    expect_equal(
      idx_r(x, 2, by = slice_by[i]),
      seq(ncol(x), 1, slice_by[i])
    ) |> errorfun()
  }
  
  enumerate <- enumerate + 1L
  
}


x <- matrix(1:50, nrow = 10)
slice_start <- c(
  1, 2, -1, -2
) * 1i
seq_start <- c(
  1, 2, nrow(x), nrow(x) - 1
)
slice_end <- c(
  1, 2, -1, -2
) * 1i
seq_end <- c(
  1, 2, nrow(x), nrow(x) - 1
)
slice_by <- c(
  1, -1, 2, -2
)

for(i in seq_along(slice_start)) {
  for(j in seq_along(slice_end)) {
    for(k in seq_along(slice_by)) {
      seq_by <- ifelse(seq_start[i] > seq_end[j], -1, 1) * abs(slice_by[k])
      
      expect_equal(
        idx_r(x, 1, slice_start[i], slice_end[j], slice_by[k]),
        seq(seq_start[i], seq_end[j], seq_by)
      ) |> errorfun()
      
      enumerate <- enumerate + 1L
      
    }
  }
}

for(i in seq_along(slice_by)) {
  if(slice_by[i] > 0) {
    expect_equal(
      idx_r(x, 1, by = slice_by[i]),
      seq(1, nrow(x), slice_by[i])
    ) |> errorfun()
  }
  else {
    expect_equal(
      idx_r(x, 1, by = slice_by[i]),
      seq(nrow(x), 1, slice_by[i])
    ) |> errorfun()
  }
  
  enumerate <- enumerate + 1L
  
}

################################################################################
# multiple array arguments ====

x <- array(1:125, c(5, 5, 5))
n <- 5
slice_start <- c(
  1, 2, -1, -2
) * 1i
seq_start <- c(
  1, 2, n, n - 1
)
slice_end <- c(
  1, 2, -1, -2
) * 1i
seq_end <- c(
  1, 2, n, n - 1
)
slice_by <- c(
  1, -1, 2, -2
)

for(i in seq_along(slice_start)) {
  for(j in seq_along(slice_end)) {
    for(k in seq_along(slice_by)) {
      seq_by <- ifelse(seq_start[i] > seq_end[j], -1, 1) * abs(slice_by[k])
      
      expect_equal(
        idx_r(x, 1:3, slice_start[i], slice_end[j], slice_by[k]),
        rep(list(seq(seq_start[i], seq_end[j], seq_by)), 3)
      ) |> errorfun()
      
      expect_equal(
        idx_r(x, 1:3, rep(slice_start[i], 3), rep(slice_end[j], 3), rep(slice_by[k], 3)),
        rep(list(seq(seq_start[i], seq_end[j], seq_by)), 3)
      ) |> errorfun()
      
      
      enumerate <- enumerate + 2L
      
    }
  }
}

for(i in seq_along(slice_by)) {
  if(slice_by[i] > 0) {
    expect_equal(
      idx_r(x, 1:3, by = slice_by[i]),
      mapply(seq, rep(1, 3), dim(x), rep(slice_by[i], 3), SIMPLIFY = FALSE)
    ) |> errorfun()
  }
  else {
    expect_equal(
      idx_r(x, 1:3, by = slice_by[i]),
      mapply(seq, dim(x), rep(1, 3), rep(slice_by[i], 3), SIMPLIFY = FALSE)
    ) |> errorfun()
  }
  
  enumerate <- enumerate + 1L
  
}


x <- array(1:prod(5:8), 5:8)

expect_equal(
  idx_r(x, 1:4, by = c(-1, 1, 2, -2)),
  n(5:1, 1:6, c(1, 3, 5, 7), c(8, 6, 4, 2))
)
enumerate <- enumerate + 1L

expect_equal(
  idx_r(x, 1:4, c(1, 2, -1, -2) * 1i, c(-1, -2, 2, 1) * 1i),
  n(1:5, 2:5, 7:2, 7:1)
)
enumerate <- enumerate + 1L

expect_equal(
  idx_r(x, 1:4, c(1, 2, -1, -2) * 1i, c(-1, -2, 2, 1) * 1i, c(1, -1, 2, -2)),
  n(1:5, 2:5, seq(7, 2, -2), seq(7, 1, -2))
)
enumerate <- enumerate + 1L



################################################################################
# data.frame arguments ====

x <- data.frame(a = 1:10, b = letters[1:10])

slice_start <- c(
  1, 2, -1, -2
) * 1i
seq_start <- c(
  1, 2, nrow(x), nrow(x) - 1
)
slice_end <- c(
  1, 2, -1, -2
) * 1i
seq_end <- c(
  1, 2, nrow(x), nrow(x) - 1
)
slice_by <- c(
  1, -1, 2, -2
)

for(i in seq_along(slice_start)) {
  for(j in seq_along(slice_end)) {
    for(k in seq_along(slice_by)) {
     seq_by <- ifelse(seq_start[i] > seq_end[j], -1, 1) * abs(slice_by[k])
      
      
      expect_equal(
        idx_r(x,1, slice_start[i], slice_end[j], slice_by[k]),
        seq(seq_start[i], seq_end[j], seq_by)
      ) |> errorfun()
      
      enumerate <- enumerate + 1L
      
    }
  }
}


for(i in seq_along(slice_by)) {
  if(slice_by[i] > 0) {
    expect_equal(
      idx_r(x, 2, by = slice_by[i]),
      seq(1, ncol(x), slice_by[i])
    ) |> errorfun()
  }
  else {
    expect_equal(
      idx_r(x, 2, by = slice_by[i]),
      seq(ncol(x), 1, slice_by[i])
    ) |> errorfun()
  }
  
  enumerate <- enumerate + 1L
  
}



x <- data.frame(
  a = 1:10, b = letters[1:10], c = factor(letters[1:10]), d = rnorm(10), e = sample(c(TRUE, FALSE, NA), 10, TRUE)
)
slice_start <- c(
  1, 2, -1, -2
) * 1i
seq_start <- c(
  1, 2, ncol(x), ncol(x) - 1
)
slice_end <- c(
  1, 2, -1, -2
) * 1i
seq_end <- c(
  1, 2, ncol(x), ncol(x) - 1
)
slice_by <- c(
  1, -1, 2, -2
)

for(i in seq_along(slice_start)) {
  for(j in seq_along(slice_end)) {
    for(k in seq_along(slice_by)) {
      seq_by <- ifelse(seq_start[i] > seq_end[j], -1, 1) * abs(slice_by[k])
      
      expect_equal(
        idx_r(x, 2, slice_start[i], slice_end[j], slice_by[k]),
        seq(seq_start[i], seq_end[j], seq_by)
      ) |> errorfun()
      
      enumerate <- enumerate + 1L
      
    }
  }
}


for(i in seq_along(slice_by)) {
  if(slice_by[i] > 0) {
    expect_equal(
      idx_r(x, 1, by = slice_by[i]),
      seq(1, nrow(x), slice_by[i])
    ) |> errorfun()
  }
  else {
    expect_equal(
      idx_r(x, 1, by = slice_by[i]),
      seq(nrow(x), 1, slice_by[i])
    ) |> errorfun()
  }
  
  enumerate <- enumerate + 1L
  
}

