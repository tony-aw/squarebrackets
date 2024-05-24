
if ( requireNamespace("tinytest", quietly = TRUE) ){
  
  env_var_is_true <- function(x) {
    # this function is borrowed from the 'testthat' package
    isTRUE(as.logical(Sys.getenv(x, "false")))
  }
  
  tinytest::test_package(
    "squarebrackets", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/special", set_env=list(LC_COLLATE="C")
  )
  # the next tests take about 10 minutes
  # (these are almost 100,000 tests)
  # and so these are disabled on CRAN:
  if(!interactive() && !env_var_is_true("NOT_CRAN")) {
    tinytest::test_package(
      "squarebrackets", testdir = "tinytest/generic", set_env=list(LC_COLLATE="C")
    )
    tinytest::test_package(
      "squarebrackets", testdir = "tinytest/generic2", set_env=list(LC_COLLATE="C")
    )
    tinytest::test_package(
      "squarebrackets", testdir = "tinytest/generic_idx", set_env=list(LC_COLLATE="C")
    )
  }
}


