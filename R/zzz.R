.onAttach <- function(libname, pkgname) {
  txt <- paste0(
    "Run `",
    '?squarebrackets::squarebrackets_help',
    "` to open the introduction help page of 'squarebrackets'."
  )
  packageStartupMessage(txt)
}

.pkgenv_squarebrackets <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
  options(
    squarebrackets.cn = FALSE,
    squarebrackets.chkdup = FALSE,
    squarebrackets.ma_messages = FALSE
  )
  .pkgenv_squarebrackets[["protected"]] <- .protected_addresses()
}
