library(testthat)
library(jute)

# nolint start
# example devtools::test calls:
#   devtools::test()
#   devtools::test(filter = ".*misc.*")
# nolint end

# nolint start
#
#   # some useful things to note:
#
#   # goodpractice::goodpractice() # -> check all aspects of package
#
#   # pkgdown::build_site() # -> build the pkgdown website
#
#   # usethis::use_testthat() # -> initialise test dir structure
#   # usethis::use_test()     # -> initialize test file and open for editing
#
# nolint end

testthat::test_check("jute")
