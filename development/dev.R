# Packages development.
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
devtools::install_github("hadley/devtools")
install.packages("rstudioapi")
library(devtools)
devtools::has_devel()
sessionInfo()
# Mulea uses.
install.packages(c("DBI", "RSQLite"))
