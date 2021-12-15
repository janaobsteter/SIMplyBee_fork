library(package = "devtools")
library(package = "AlphaSimR")

devtools::load_all() # shortcut = shit + Ctrl + L) loads the package - this can print some conflicts
devtools::build() # (shortcut = shit + Ctrl + B) builds the package (new tar.gz) - will fail if there are some conflicts
devtools::test() # (shortcut = shit + Ctrl + T) runs the tests!
devtools::document() # write/modifies the documentation
usethis::use_test("name") # creates a test file in the test/testthat folder
