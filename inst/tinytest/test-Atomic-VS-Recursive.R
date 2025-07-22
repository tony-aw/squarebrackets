
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


x <- as.list(1:10)
pattern <- "Use the `i2_` methods for recursive objects"

meths <- list(
  ii_x, ii_wo, ii_mod
)
for(i in seq_along(meths)) {
  expect_error(
    meths[[i]](x, i = 1),
    pattern = pattern,
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
}


x <- array(as.list(1:27), c(3,3,3))
pattern <- "Use the `ss2_` methods for recursive objects"

meths <- list(
  ss_x, ss_wo, ss_mod
)
for(i in seq_along(meths)) {
  expect_error(
    meths[[i]](x, 1:2),
    pattern = pattern,
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
}


x <- 1:10
pattern <- "Use the `i_` methods for atomic objects"

meths <- list(
  ii2_x, ii2_wo, ii2_mod
)
for(i in seq_along(meths)) {
  expect_error(
    meths[[i]](x, i = 1),
    pattern = pattern,
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
}



x <- array(1:27, c(3,3,3))
pattern <- "Use the `ss_` methods for atomic objects"

meths <- list(
  ss2_x, ss2_wo, ss2_mod
)
for(i in seq_along(meths)) {
  expect_error(
    meths[[i]](x, 1:2),
    pattern = pattern,
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
}

