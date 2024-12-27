
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
    1 * -1i,
    1:3 * -1i,
    3:1 * -1i,
    c(2, 3, 1) * -1i,
    1 * 1i,
    1:3 * 1i,
    3:1 * 1i,
    c(2, 3, 1) * 1i
  )
  if(test_allow_duplicates) {
    out <- c(out, list(c(1, 1, 1)))
  }
  return(out)
}

indx_named <- function(x, dim.i) {
  if(dim.i == 1L) {
    out <- indx_general(x, dim.i)
  }
  if(dim.i == 2L) {
    out <- c(
      indx_general(x, dim.i), list("a", c("a", "b"), c("b", "a"))
    )
  }
  
  return(out)
}


temp.fun.main <- function(x, row, col, f_expect, f_out) {
  
  out <- expected <- vector(
    "list", length(row) * length(col)
  )
  counter <- 1
  
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      
      s <- n(row[[i]], col[[j]])
      d <- 1:2
      rem <- which(vapply(s, is.null, logical(1L)))
      if(length(rem) > 0L) {
        s <- s[-rem]
        d <- d[-rem]
      }
      
      rp <- lapply(pre_subset_df(x, s, d), \(x)x[1])
      expected[[counter]] <- f_expect(x, row[[i]], col[[j]])
      out[[counter]] <- f_out(x, s, d)
      assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
      counter <- counter + 1
    }
  }
  counter <- counter - 1
  out <- out[1:counter]
  expected <- expected[1:counter]
  
  expect_equal(expected, out) |> errorfun()
  expect_true(all(sapply(out, is.data.frame))) |> errorfun()
}


# indices ====
x.sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
n <- nrow(x.sf)
x.extra <- data.frame(
  a = - sample.int(n),
  b = sample(letters, n, TRUE),
  c = rnorm(n),
  d = sample(c(TRUE, FALSE), n, TRUE)
)
x.original <- cbind(x.extra, x.sf)
row <- indx_general(x.original, 1)
col <- indx_general(x.original, 2)
filter <- list(
  NULL,
  ~ (a >= 4) & (b != "a") & (d==TRUE), # partially TRUE/FALSE
  ~ (a > 10) | (b == "z"), # completely FALSE
  ~ (a < 10) | (b == "a") # completely TRUE
)
get_vars <- list(
  NULL,
  is.double, # partially TRUE/FALSE
  is.factor, # completely FALSE
  is.atomic # completely TRUE
)



if(!test_PassByReference) {
  # data.frame ====
  x <- as.data.frame(x.original)
  
  temp.fun.main(x, row, col, f_expect.data.frame, f_out.data.frame)
  
  if(isTRUE(test_allow_duplicates)) {
    expect_equal(
      anyDuplicated(colnames(sb2_x(x, s = c(1,1,1), d = 2L))),
      0
    )
    expect_equal(
      anyDuplicated(colnames(sb2_x(x, s = c("a","a","a"), d = 2L))),
      0
    )
    enumerate <- enumerate <- enumerate + 1
  }
  
  
  # tibble ====
  if(requireNamespace("tibble")) {
    x <- tibble::as_tibble(x.original)
    
    temp.fun.main(x, row, col, f_expect.data.frame, f_out.data.frame)
    
    
    if(isTRUE(test_allow_duplicates)) {
      expect_equal(
        anyDuplicated(colnames(sb2_x(x, s = c(1,1,1), d = 2L))),
        0
      )
      expect_equal(
        anyDuplicated(colnames(sb2_x(x, s = c("a","a","a"), d = 2L))),
        0
      )
      enumerate <- enumerate <- enumerate + 1
    }
  }
  
  
}


# data.table ====
if(requireNamespace("data.table")) {
  
  x <- data.table::as.data.table(x.original)
  temp.fun.main(x, row, col, f_expect.data.frame, f_out.data.frame)
  
  
  if(isTRUE(test_allow_duplicates)) {
    expect_equal(
      anyDuplicated(colnames(sb2_x(x, s = c(1,1,1), d = 2L))),
      0
    )
    expect_equal(
      anyDuplicated(colnames(sb2_x(x, s = c("a","a","a"), d = 2L))),
      0
    )
    enumerate <- enumerate <- enumerate + 1
  }
  
}





# tidytable ====
if(requireNamespace("tidytable")) {
  x <- tidytable::as_tidytable(x.original)
  
  temp.fun.main(x, row, col, f_expect.data.frame, f_out.data.frame)
  
  
  if(isTRUE(test_allow_duplicates)) {
    expect_equal(
      anyDuplicated(colnames(sb2_x(x, s = c(1,1,1), d = 2L))),
      0
    )
    expect_equal(
      anyDuplicated(colnames(sb2_x(x, s = c("a","a","a"), d = 2L))),
      0
    )
    enumerate <- enumerate <- enumerate + 1
  }
  
}

