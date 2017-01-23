# Packages development.
install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown"))
devtools::install_github("hadley/devtools", force = TRUE)
install.packages("rstudioapi")
library(devtools)
devtools::has_devel()
sessionInfo()

# Build with vignetts.
devtools::use_vignette(name = "MulEA")
devtools::use_vignette(name = "test")
devtools::build_vignettes()
vignette("test")
devtools::use_vignette(name = "MulEA")
devtools::build(binary = TRUE, args = c('--preclean'))
devtools::build("/home/koralgooll/doktorat/Rpackages/Mulea/")
devtools::install()
devtools::reload()
dev
pkgbuild::build(dest_path = ".")
getwd()
install("pkgbuild")
library(pkgbuild)
library(pkgload)
library(pkgmaker)
# Source package is supported on Linux.
install.packages("/home/koralgooll/doktorat/Rpackages/mulea/Mulea_0.99.0.tar.gz"
                 , repos = NULL, type = "source")
# Binary is supported only on Windows and OS X.
install.packages("/home/koralgooll/doktorat/Rpackages/mulea/Mulea_0.99.0_R_x86_64-pc-linux-gnu.tar.gz"
                  , repos = NULL, type = "binary")
vignette("MulEA")
#

# Mulea uses.
install.packages(c("DBI", "RSQLite"))

# R things.
R.Version()

# BioConductor important steps.
# IMPORTANT note : if some errors restart session or restard IDE!
source("https://bioconductor.org/biocLite.R")
library(BiocInstaller)
BiocInstaller::useDevel()
BiocInstaller::biocValid()
BiocInstaller::biocLite()
# R CMD BiocCheck
biocLite("BiocCheck")
library(BiocCheck)
#Do BiocCheck on development version of package, not installed. :P
BiocCheck("/home/koralgooll/doktorat/Rpackages/mulea/Mulea")
#Not this directory:
find.package("Mulea")
# KNOWN BUG
#Failed to copy the script/BiocCheck script to /usr/lib/R/bin. If you want to be able to
#run 'R CMD BiocCheck' you'll need to copy it yourself to a directory on your PATH, making
#sure it is executable. See the BiocCheck vignette for more information.
# SOLUTION
find.package("BiocCheck")
#Do what is in instruction. Copy that file please. :)


# WORK HELPERS
# Clean screen.
cat("\014")
packageVersion("topGO")
