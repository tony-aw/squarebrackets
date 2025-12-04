
if(test_PassByReference) {
  if(requireNamespace("tidytable")) {
    xlist <- list(
      dt = data.table::data.table(a = 1:26, b = letters),
      tt = tidytable::tidytable(a = 1:26, b = letters)
    )
  } else {
    xlist <- list(
      dt = data.table::data.table(a = 1:26, b = letters)
    )
  }
  
}
if(!test_PassByReference) {
  if(requireNamespace("tibble") && requireNamespace("tidytable")) {
    xlist <- list(
      df = data.frame(a = 1:26, b = letters),
      dt = data.table::data.table(a = 1:26, b = letters),
      tb = tibble::tibble(a = 1:26, b = letters),
      tt = tidytable::tidytable(a = 1:26, b = letters)
    )
  } else {
    xlist <- list(
      df = data.frame(a = 1:26, b = letters),
      dt = data.table::data.table(a = 1:26, b = letters),
    )
  }
  
}

for(i in seq_along(xlist)) {
  x <- xlist[[i]]
  expect_error(
    sb_test(x, obs = "a"),
    pattern = "incorrect index type"
  )
  expect_error(
    sb_test(x, col = list(1:10)),
    pattern = "incorrect index type"
  )
  enumerate <- enumerate + 2L
}

