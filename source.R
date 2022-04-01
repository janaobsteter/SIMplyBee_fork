
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
