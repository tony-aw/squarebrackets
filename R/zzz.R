

.pkgenv_mutatomic <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
  options(
    squarebrackets.cn = FALSE,
    squarebrackets.chkdup = FALSE,
    squarebrackets.ma_messages = FALSE,
    squarebrackets.sticky = c("difftime", "Date", "POSIXct", "roman", "hexmode", "octmode", "broadcaster")
  )
  .pkgenv_mutatomic[["protected"]] <- .protected_addresses()
}


.onAttach <- function(libname, pkgname) {
  txt <- paste0(
    "Run `",
    '?squarebrackets::squarebrackets_help',
    "` to open the introduction help page of 'squarebrackets'."
  )
  packageStartupMessage(txt)
}



