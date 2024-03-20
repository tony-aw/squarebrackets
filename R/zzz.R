.onAttach <- function(libname, pkgname) {
  txt <- paste0(
    "Run `",
    '?squarebrackets::subsets',
    "` to open the introduction help page of 'squarebrackets'."
  )
  packageStartupMessage(txt)
}

.onLoad <- function(libname, pkgname) {
  options(
    squarebrackets.protected = .protected_addresses(),
    sb.rat = FALSE,
    sb.chkdup = FALSE
  )
}