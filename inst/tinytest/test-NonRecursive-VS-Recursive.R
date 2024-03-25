enumerate <- 0

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


x <- as.list(1:10)
pattern <- "Use the `sb2_` methods for recursive objects"

meths <- list(
  sb_x, sb_rm, sb_mod, sb_coe, sb_before, sb_after
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
pattern <- "Use the `sb_` methods for non-recursive objects"

meths <- list(
  sb2_x, sb2_rm, sb2_mod, sb2_coe, sb2_before, sb2_after
)
for(i in seq_along(meths)) {
  expect_error(
    meths[[i]](x, i = 1),
    pattern = pattern,
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
}



