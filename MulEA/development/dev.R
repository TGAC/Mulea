###### Depndencies ######
source("http://bioconductor.org/biocLite.R")
# Ubuntu system dependencies
# sudo apt-get install libssl-dev
# sudo apt-get install libxml2-dev

# development dependencies
biocLite(c("devtools", "roxygen2", "knitr", "rmarkdown"))
## Can include important bug fixes.
devtools::install_github("hadley/devtools", force = TRUE)

# build dependencies
biocLite(c("DBI", "RSQLite", "topGO", "fgsea"))

# test dependencies
biocLite("testthat")


###### Vignete Build ######
devtools::use_vignette(name = "MulEA")
devtools::build_vignettes()
vignette("MulEA")


###### Unit Tests ######
library(testthat)
devtools::use_testthat()
devtools::test()


###### Bioconductor - Package Submission ######


###### Integration Tests ######

# CRAN -> R CMD check
library(devtools)
devtools::check()

# Bioconducto -> R CMD BiocCheck
source("https://bioconductor.org/biocLite.R")
biocLite("BiocCheck")
library(BiocCheck)
#Do BiocCheck on development version of package, not installed. :P
BiocCheck::BiocCheck(getwd())


###### Work Helpers ######
sessionInfo()
R.Version()
cat("\014")
packageVersion("topGO")
find.package("Mulea")


###### ??? ######
devtools::build(binary = TRUE, args = c('--preclean'))
devtools::build("/home/koralgooll/doktorat/Rpackages/Mulea/")
devtools::install()
devtools::reload()
install.packages("rstudioapi")
devtools::has_devel()


###### Known Bugs ######

# IMPORTANT note : if some errors restart session or restard IDE!

#Failed to copy the script/BiocCheck script to /usr/lib/R/bin. If you want to be able to
#run 'R CMD BiocCheck' you'll need to copy it yourself to a directory on your PATH, making
#sure it is executable. See the BiocCheck vignette for more information.
# SOLUTION
find.package("BiocCheck")
#Do what is in instruction. Copy that file please. :)
