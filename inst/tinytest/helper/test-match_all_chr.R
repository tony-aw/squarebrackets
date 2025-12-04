
enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())

general_names <- combn(letters, 2) |> apply(2, paste, collapse = "")

needles <- list(
  c("a", "c"),
  "ab",
  c("ac", "ab", "ab", "ac"), # to check duplicates AND ordering
  character(0),
  general_names,
  c(general_names, "ab", "ab"),
  letters,
  LETTERS
)
haystack <- list(
  c("a", "c"),
  "ab",
  c("ac", "ab", "ab", "ac"),
  NA,
  character(0),
  general_names,
  c(general_names, "ab", "ab", NA),
  letters,
  LETTERS
)

tempfun <- function(needles, haystack) {
  if(length(na.omit(needles)) == 0 || length(na.omit(haystack)) == 0) {
    return(integer(0))
  }
  return(lapply(na.omit(needles), \(j) which(na.omit(haystack) == j)) |> unlist())
}

for(iNeedles in needles) {
  for(iHaystack in haystack) {
    expect_equal(
      tempfun(iNeedles, iHaystack),
      match_all(iNeedles, iHaystack)
    ) |> errorfun()
    enumerate <- enumerate + 1
  }
}

tempfun <- function(needles, haystack) {
  if(length(na.omit(needles)) == 0 || length(na.omit(haystack)) == 0) {
    return(list())
  }
  out <- lapply(needles, \(j) which(haystack == j))
  names(out) <- needles
  return(out)
}

checkfun <- function(x, y) {
  if(length(x)==0 && length(y) == 0) return(TRUE)
  if(collapse::allNA(x) && collapse::allNA(y)) {
    return(length(x) == length(y))
  }
  check <- all(x == y, na.rm = TRUE) && all(is.na(x) == is.na(y))
  return(check)
}

for(iNeedles in needles) {
  for(iHaystack in haystack) {
    
    
    expect <- tempfun(iNeedles, iHaystack)
    out <- match_all(iNeedles, iHaystack, unlist = FALSE)
    
    if(length(expect) != 0 || length(out) != 0) {
      
      expect_true(
        all(mapply(checkfun, expect, out))
      ) |> errorfun() |> print()
    }
    enumerate <- enumerate + 1
    
  }
}

