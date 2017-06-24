# Title     : TODO
# Objective : TODO
# Created by: cezary
# Created on: 23.06.17

if (!("devtools" %in% rownames(installed.packages()))) {
    install.packages("devtools")
}
require(devtools)
build()
install()