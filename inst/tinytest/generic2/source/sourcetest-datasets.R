
# set-up ====

any_empty_indices <- function(...) {
  lst <- list(...)
  return(squarebrackets:::.any_empty_indices(lst))
}


indx_general <- function(x, dim.i) {
  dim.n <- dim(x)[[dim.i]]
  dim.n1 <- dim.n - round(dim.n/2)
  dim.n2 <- dim.n - dim.n1
  out <- list(
    NULL,
    logical(0),
    rep(TRUE, dim.n),
    rep(FALSE, dim.n),
    c(rep(TRUE, dim.n1), rep(FALSE, dim.n2)),
    1,
    1:3,
    3:1,
    c(2, 3, 1),
    1 - 1i,
    1:3 - 1i,
    3:1 - 1i,
    c(2, 3, 1) - 1i,
    1 + 1i,
    1:3 + 1i,
    3:1 + 1i,
    c(2, 3, 1) + 1i
  )
  if(test_allow_duplicates) {
    out <- c(out, list(c(1, 1, 1)))
  }
  return(out)
}

indx_named <- function(x, dim.i) {
  if(dim.i == 1L) {
    out <- c(
      indx_general(x, dim.i),
      list("1", as.character(1:3), as.character(3:1), c("1", "3", "2"))
    )
  }
  if(dim.i == 2L) {
    out <- c(
      indx_general(x, dim.i), list("a", c("a", "b"), c("b", "a"))
    )
  }
  
  return(out)
}


temp.fun.main <- function(x, row, col, filter, get_vars, f_expect, f_out) {
  
  out <- expected <- vector(
    "list", length(row) * length(col) * length(filter) * length(get_vars) / 2L
  )
  counter <- 1
  tracker <- matrix(0L, length(out), 4L)
  
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      for(k in 1:length(filter)) {
        for(l in 1:length(get_vars)) {
          wrong1 <- is.null(row[[i]]) && is.null(col[[j]]) && is.null(filter[[k]]) && is.null(get_vars[[l]])
          wrong2 <- !is.null(filter[[k]]) && !is.null(row[[i]])
          wrong3 <- !is.null(get_vars[[l]]) && !is.null(col[[j]])
          if(!wrong1 && !wrong2 && !wrong3) {
           tracker[counter, ] <- c(i, j, k, l)
            
            rp <- lapply(pre_subset_df(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]]), \(x)x[1])
            
            expected[[counter]] <- f_expect(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]])
            out[[counter]] <- f_out(x, row = row[[i]], col = col[[j]], filter[[k]], get_vars[[l]])
            assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
            counter <- counter + 1
          }
        }
      }
    }
  }
  counter <- counter - 1
  out <- out[1:counter]
  expected <- expected[1:counter]
  tracker <- tracker[1:counter, ]
  
  expect_equal(expected, out) |> errorfun()
  expect_true(all(sapply(out, is.data.frame))) |> errorfun()
}


# indices ====
x.original <- data.frame(
  a = -sample.int(10),
  b = letters[1:10],
  c = rnorm(10),
  d = rep(c(TRUE, FALSE), 5)
)
row <- indx_named(x.original, 1)
col <- indx_named(x.original, 2)
filter <- list(
  NULL,
  ~ (a >= 4) & (b != "a") & (d==TRUE), # partially TRUE/FALSE
  ~ (a > 10) | (b == "z"), # completely FALSE
  ~ (a < 10) | (b == "a") # completely TRUE
)
get_vars <- list(
  NULL,
  is.numeric, # partially TRUE/FALSE
  is.factor, # completely FALSE
  is.atomic # completely TRUE
)


if(!test_PassByReference) {
  # data.frame ====
  x <- as.data.frame(x.original)
  
  temp.fun.main(x, row, col, filter, get_vars, f_expect.data.frame, f_out.data.frame)
  
  if(isTRUE(test_allow_duplicates)) {
    expect_equal(
      anyDuplicated(colnames(sb2_x(x, col = c(1,1,1)))),
      0
    )
    expect_equal(
      anyDuplicated(colnames(sb2_x(x, col = c("a","a","a")))),
      0
    )
    enumerate <- enumerate <- enumerate + 1
  }
  
  
  # tibble ====
  if(requireNamespace("tibble")) {
    x <- tibble::as_tibble(x.original)
    
    temp.fun.main(x, row, col, filter, get_vars, f_expect.data.frame, f_out.data.frame)
    
    
    if(isTRUE(test_allow_duplicates)) {
      expect_equal(
        anyDuplicated(colnames(sb2_x(x, col = c(1,1,1)))),
        0
      )
      expect_equal(
        anyDuplicated(colnames(sb2_x(x, col = c("a","a","a")))),
        0
      )
      enumerate <- enumerate <- enumerate + 1
    }
  }
  
  
}


# data.table ====
if(requireNamespace("data.table")) {
  
  x <- dt.$as.data.table(x.original)
  temp.fun.main(x, row, col, filter, get_vars, f_expect.data.frame, f_out.data.frame)
  
  
  if(isTRUE(test_allow_duplicates)) {
    expect_equal(
      anyDuplicated(colnames(sb2_x(x, col = c(1,1,1)))),
      0
    )
    expect_equal(
      anyDuplicated(colnames(sb2_x(x, col = c("a","a","a")))),
      0
    )
    enumerate <- enumerate <- enumerate + 1
  }
  
}





# tidytable ====
if(requireNamespace("tidytable")) {
  x <- tidytable::as_tidytable(x.original)
  
  temp.fun.main(x, row, col, filter, get_vars, f_expect.data.frame, f_out.data.frame)
  
  
  if(isTRUE(test_allow_duplicates)) {
    expect_equal(
      anyDuplicated(colnames(sb2_x(x, col = c(1,1,1)))),
      0
    )
    expect_equal(
      anyDuplicated(colnames(sb2_x(x, col = c("a","a","a")))),
      0
    )
    enumerate <- enumerate <- enumerate + 1
  }
  
}

