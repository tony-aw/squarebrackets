.onAttach <- function(libname, pkgname) {
  txt <- paste0(
    "Run `",
    '?squarebrackets::squarebrackets_help',
    "` to open the introduction help page of 'squarebrackets'."
  )
  packageStartupMessage(txt)
}

.onLoad <- function(libname, pkgname) {
  options(
    squarebrackets.protected = .protected_addresses(),
    squarebrackets.cn = FALSE,
    squarebrackets.chkdup = FALSE,
    squarebrackets.ma_messages = TRUE
  )
}
