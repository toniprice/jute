library(testthat)
library(jute)

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
#   # https://stackoverflow.com/questions/55359837/
#       r-cmd-build-versus-devtoolsbuild-non-standard-file-
#       directory-found-at-top-l
#   # Rscript -e "devtools::document();devtools::check();devtools::build()"
#
# nolint end

# nolint start
# example devtools::test calls:
#   devtools::test()                       # (Ctrl + Shift + T)
#   devtools::test(filter = ".*misc.*")
# nolint end

testthat::test_check("jute")
