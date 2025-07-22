
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


# data.frame error ====
pattern <- "Use the `ss2_` methods for data.frame-like objects"
x <- data.frame(letters, LETTERS)
meths <- list(
  ii2_x, ii2_wo, ii2_mod
)
for(i in seq_along(meths)) {
  expect_error(
    meths[[i]](x, 1),
    pattern = pattern,
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1L
}


# non-dimensional error ====
pattern <- "cannot use the `ss_`/`ss2_` methods on non-dimensional objects"
x <- 1:10
meths <- list(
  ss_x, ss_wo, ss_mod
)
for(i in seq_along(meths)) {
  expect_error(
    meths[[i]](x, 1),
    pattern = pattern,
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1L
}

pattern <- "cannot use the `ss_`/`ss2_` methods on non-dimensional objects"
x <- as.list(1:10)
meths <- list(
  ss2_x, ss2_wo, ss2_mod
)
for(i in seq_along(meths)) {
  expect_error(
    meths[[i]](x, 1),
    pattern = pattern,
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1L
}


