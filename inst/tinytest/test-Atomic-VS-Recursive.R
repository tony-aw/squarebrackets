
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


x <- as.list(1:10)
pattern <- "Use the `sb2_` methods for recursive objects"

meths <- list(
  sb_x, sb_wo, sb_mod
)
for(i in seq_along(meths)) {
  expect_error(
    meths[[i]](x, i = 1),
    pattern = pattern,
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
}


x <- 1:10
pattern <- "Use the `sb_` methods for atomic objects"

meths <- list(
  sb2_x, sb2_wo, sb2_mod
)
for(i in seq_along(meths)) {
  expect_error(
    meths[[i]](x, i = 1),
    pattern = pattern,
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
}



