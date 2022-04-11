
rm(list = ls())

# install.packages(pkg = c("devtools", "styler"))
library(package = "devtools")
library(package = "styler")

devtools::load_all() # shortcut = shit + Ctrl + L) loads the package - this can print some conflicts
devtools::build() # (shortcut = shit + Ctrl + B) builds the package (new tar.gz) - will fail if there are some conflicts

usethis::use_test("name") # creates a test file in the test/testthat folder
devtools::test() # (shortcut = shit + Ctrl + T) runs the tests!

styler::style_pkg()
devtools::document() # (shortcut = shit + Ctrl + D) write/modifies the documentation
devtools::check() # (shortcut = shit + Ctrl + E) R CMD check

devtools::install()

devtools::install_github(repo="HighlanderLab/SIMplyBee@main",
                         subdir = "SIMplyBee")

devtools::install_github(repo="HighlanderLab/SIMplyBee@devel",
                         subdir = "SIMplyBee")

library(SIMplyBee)

# Find all functions and their arguments (in SIMplyBee and imported packages)
objects <- ls(name = "package:SIMplyBee")
for (object in objects) {
  # object <- "isDrone"
  code <- get(x = object)
  if (is.function(code)) {
    tmp <- formals(code)
    if ("simParamBee" %in% names(tmp)) {
      cat(paste(object, paste(names(tmp), collapse = " "), "\n"))
    }
  }
}

# Find all functions and their arguments (in AlphaSimR)
objects <- ls(name = "package:AlphaSimR")
for (object in objects) {
  # object <- "isDrone"
  code <- get(x = object)
  if (is.function(code)) {
    tmp <- formals(code)
    if ("simParam" %in% names(tmp)) {
      cat(paste(object, paste(names(tmp), collapse = " "), "\n"))
    }
  }
}

# Find all functions and their arguments (in SIMplyBee and imported packages) - with defaults
objects <- ls(name = "package:SIMplyBee")
sink(file = "SIMplyBee_functions_and_arguments.txt")
for (object in objects) {
  # object <- "isDrone"
  code <- get(x = object)
  if (is.function(code)) {
    tmp <- formals(code)
    cat(object, "\n")
    cat(str(tmp))
    cat("\n")
  }
}
sink()

# Find TODOs
system(command = "grep TODO *")
system(command = "grep TODO */*")
system(command = "grep TODO */*/*")
system(command = "grep TODO */*/*/*")
