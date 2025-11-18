
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


# use sbt_ for data.frames ====
x <- data.table::data.table(a = 1:10, b = letters[1:10])
pattern <- "Use the `sbt_` methods for data.frames"

meths <- list(
  ii_x, ii_wo, ii_mod, ii_set, ss_x, ss_wo, ss_mod, ss_set
)
for(i in seq_along(meths)) {
  expect_error(
    meths[[i]](x, i = 1),
    pattern = pattern,
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
}


# use ii_ for non-dimensional objects ====

x <- as.mutatomic(1:10)
pattern <- "Use the `ii_` methods for non-dimensional objects"

meths <- list(
  ss_x, ss_wo, ss_mod, ss_set, sbt_x, sbt_wo, sbt_mod, sbt_set
)
for(i in seq_along(meths)) {
  expect_error(
    meths[[i]](x, i = 1),
    pattern = pattern,
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
}


# use ss_ for for dimensional objects with ndim(x) != 2L

x <- mutatomic(1:27, dim = c(3,3,3))
pattern <- "Use the `ss_` methods for dimensional objects with ndim(x) != 2L"

meths <- list(
  sbt_x, sbt_wo, sbt_mod, sbt_set
)
for(i in seq_along(meths)) {
  expect_error(
    meths[[i]](x, i = 1),
    pattern = pattern,
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
}


