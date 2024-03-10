# count number of tests
library(squarebrackets)
library(tinytest)

# set working directory to source file location ====
SourceFileLocation <- function() {
  # BATCH way:
  path <- funr::get_script_path()
  if(!is.null(path)) return(path)
  
  # R-Studio way:
  if(Sys.getenv("RSTUDIO") == "1") {
    if(rstudioapi::isAvailable(version_needed = NULL,child_ok = FALSE)) {
      return(dirname(rstudioapi::getSourceEditorContext()$path))
    }
    if(is.null(knitr::current_input(dir = TRUE)) == FALSE) {
      return(knitr::current_input(dir = TRUE))
    }
    return(getwd())
  }
}


wd <- SourceFileLocation()
setwd(wd)
setwd("..")
getwd()

enumerate_total <- 0
files <- list.files(normalizePath(getwd()), pattern = ".R", full.names = TRUE)
max.width <- max(stringi::stri_width(basename(files))) + 8
for(iFile in files) {
  capture.output(source(normalizePath(iFile)), file = nullfile())
  cat(stringi::stri_pad_right(basename(iFile), max.width), " -> ", enumerate,  "\n")
  enumerate_total <- enumerate_total + enumerate
}

print(enumerate_total)


# end ====

