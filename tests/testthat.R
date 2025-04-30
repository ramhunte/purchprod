# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

# testing tools

# usethis::use_test() to create the test file (the first time it’s run) or navigate to the test file (if it already exists).

# Cmd/Ctrl + Shift + T  devtools::test() to make you have accidentally broken anything else.

# devtools::test_coverage()

# devtools::test_coverage_active_file()

library(testthat)
library(purchprod)

test_check("purchprod")
