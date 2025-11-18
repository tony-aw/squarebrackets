
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


temp.fun.main <- function(x, obs, vars, f_expect, f_out) {
  
  out <- expected <- vector(
    "list", length(obs) * length(vars)
  )
  counter <- 1
  
  for(i in 1:length(obs)) {
    for(j in 1:length(vars)) {
      
      rp <- lapply(pre_subset_df(x, obs = obs[[i]], vars = vars[[j]]), \(x)x[1])
      expected[[counter]] <- f_expect(x, row = obs[[i]], col = vars[[j]])
      out[[counter]] <- f_out(x, obs = obs[[i]], vars = vars[[j]])
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

temp.fun.special <- function(x, obs, vars, f_expect, f_out) {
  
  out <- expected <- vector(
    "list", length(obs) * length(vars)
  )
  counter <- 1
  
  for(i in 1:length(obs)) {
    for(j in 1:length(vars)) {
      
      rows <- squarebrackets:::.with_internal(x, obs[[i]], sys.call())
      cols <- vapply(x, vars[[j]], logical(1L))
      
      rp <- lapply(pre_subset_df(x, obs = obs[[i]], vars = vars[[j]]), \(x)x[1])
      expected[[counter]] <- f_expect(x, row = rows, col = cols)
      out[[counter]] <- f_out(x, obs = obs[[i]], vars = vars[[j]])
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
x.original <- data.frame(
  a = -sample.int(10),
  b = letters[1:10],
  c = rnorm(10),
  d = rep(c(TRUE, FALSE), 5)
)
obs <- indx_general(x.original, 1)
vars <- indx_named(x.original, 2)
filter <- list(
  ~ (a >= 4) & (b != "a") & (d==TRUE), # partially TRUE/FALSE
  ~ (a > 10) | (b == "z"), # completely FALSE
  ~ (a < 10) | (b == "a") # completely TRUE
)
get_vars <- list(
  is.numeric, # partially TRUE/FALSE
  is.factor, # completely FALSE
  is.atomic # completely TRUE
)



if(!test_PassByReference) {
  # data.frame ====
  x <- as.data.frame(x.original)
  
  temp.fun.main(x, obs, vars, f_expect.data.frame, f_out.data.frame)
  temp.fun.special(x, filter, get_vars, f_expect.data.frame, f_out.data.frame)
  
  if(isTRUE(test_allow_duplicates)) {
    expect_equal(
      anyDuplicated(colnames(sbt_x(x, vars = c(1,1,1)))),
      0
    )
    expect_equal(
      anyDuplicated(colnames(sbt_x(x, vars = c("a","a","a")))),
      0
    )
    enumerate <- enumerate <- enumerate + 1
  }
  
  
  # tibble ====
  if(requireNamespace("tibble")) {
    x <- tibble::as_tibble(x.original)
    
    temp.fun.main(x, obs, vars, f_expect.data.frame, f_out.data.frame)
    temp.fun.special(x, filter, get_vars, f_expect.data.frame, f_out.data.frame)
    
    if(isTRUE(test_allow_duplicates)) {
      expect_equal(
        anyDuplicated(colnames(sbt_x(x, vars = c(1,1,1)))),
        0
      )
      expect_equal(
        anyDuplicated(colnames(sbt_x(x, vars = c("a","a","a")))),
        0
      )
      enumerate <- enumerate <- enumerate + 1
    }
  }
  
  
}


# data.table ====
if(requireNamespace("data.table")) {
  
  x <- data.table::as.data.table(x.original)
  temp.fun.main(x, obs, vars, f_expect.data.frame, f_out.data.frame)
  temp.fun.special(x, filter, get_vars, f_expect.data.frame, f_out.data.frame)
  
  if(isTRUE(test_allow_duplicates)) {
    expect_equal(
      anyDuplicated(rownames(sbt_x(x, obs = c(1,1,1)))),
      0
    )
    expect_equal(
      anyDuplicated(colnames(sbt_x(x, vars = c("a","a","a")))),
      0
    )
    enumerate <- enumerate <- enumerate + 1
  }
  
}





# tidytable ====
if(requireNamespace("tidytable")) {
  x <- tidytable::as_tidytable(x.original)
  
  temp.fun.main(x, obs, vars, f_expect.data.frame, f_out.data.frame)
  temp.fun.special(x, filter, get_vars, f_expect.data.frame, f_out.data.frame)
  
  if(isTRUE(test_allow_duplicates)) {
    expect_equal(
      anyDuplicated(colnames(sbt_x(x, vars = c(1,1,1)))),
      0
    )
    expect_equal(
      anyDuplicated(colnames(sbt_x(x, vars = c("a","a","a")))),
      0
    )
    enumerate <- enumerate <- enumerate + 1
  }
  
}



