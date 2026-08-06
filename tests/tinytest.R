
if ( requireNamespace("tinytest", quietly = TRUE) ){
  
  tinytest::test_package(
    "squarebrackets", set_env=list(LC_COLLATE="C")
  )
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/args", set_env=list(LC_COLLATE="C")
  )
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/developer", set_env=list(LC_COLLATE="C")
  )
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/generic", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/generic2", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/generic_rename", set_env=list(LC_COLLATE="C")
  )
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/helper", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/internal", set_env=list(LC_COLLATE="C")
  )
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/mutatomic", set_env=list(LC_COLLATE="C")
  )
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/semantics", set_env=list(LC_COLLATE="C")
  )
  
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/slice_ptrn", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/slice_seq", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/slice_v", set_env=list(LC_COLLATE="C")
  )
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/special_dt", set_env=list(LC_COLLATE="C")
  )
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/special_lst", set_env=list(LC_COLLATE="C")
  )
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/src_related", set_env=list(LC_COLLATE="C")
  )
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/ss2ii", set_env=list(LC_COLLATE="C")
  )
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/stride", set_env=list(LC_COLLATE="C")
  )
  
  tinytest::test_package(
    "squarebrackets", testdir = "tinytest/zerolen", set_env=list(LC_COLLATE="C")
  )
  
}


