
if ( requireNamespace("tinytest", quietly = TRUE) ){
  
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
    "squarebrackets", testdir = "tinytest/sub2ind", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/src_related", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/developer", set_env=list(LC_COLLATE="C")
  )
  
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/slice", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/slicev", set_env=list(LC_COLLATE="C")
  )
  
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/generic", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/generic2", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/generic_lst", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/generic_idx", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/mutatomic", set_env=list(LC_COLLATE="C")
  )
  
}


