# Title     : TODO
# Objective : TODO
# Created by: cezary
# Created on: 23.06.17

if (!("testthat" %in% rownames(installed.packages()))) {
    install.packages("testthat")
}

require(testthat)
devtools::use_testthat()
devtools::test()
