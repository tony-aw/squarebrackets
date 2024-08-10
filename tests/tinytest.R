
if ( requireNamespace("tinytest", quietly = TRUE) ){
  
  ON_CRAN <- FALSE # I set this to TRUE when submitting to CRAN
  
  tinytest::test_package(
    "squarebrackets", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/special", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/helper", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/src_related", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/developer", set_env=list(LC_COLLATE="C")
  )
  
  # the next tests take about 10 minutes
  # (these are almost 100,000 tests)
  # and so these are disabled on CRAN:
  if(!ON_CRAN) {
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
  
  # the next tests explicitly require the abind package
  if(requireNamespace("abind")) {
    tinytest::test_package(
      "squarebrackets", testdir = "tinytest/generic_bind", set_env=list(LC_COLLATE="C")
    )
  }
  
}


