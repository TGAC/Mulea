## ----global_options, include=TRUE, echo=FALSE----------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, error = TRUE)

## ---- results = 'asis'---------------------------------------------------
pathToModelGmtFile <- paste(find.package("MulEA"),"/example/model.gmt", sep = "")
modelDfFromFile <- MulEA::readGmtFileAsDF(gmtFilePath = pathToModelGmtFile)
knitr::kable(modelDfFromFile, caption = "Model Data Frame")

## ---- results = 'asis'---------------------------------------------------
experimentalData <- c("FBgn0004407", "FBgn0010438", "FBgn0003742", "FBgn0029709", "FBgn0030341", "FBgn0037044", "FBgn0002887")
hypergeometricTest <- MulEA::calculateHypergeometricTest(model = modelDfFromFile, sampleVector = experimentalData)
knitr::kable(hypergeometricTest, caption = "Hypergeometric Test Result Data Frame")

## ---- results = 'asis'---------------------------------------------------
fisherTest <- MulEA::calculateFisherTest(model = modelDfFromFile, sampleVector = experimentalData)
knitr::kable(fisherTest, caption = "Fisher Test Result Data Frame")

## ---- results = 'asis'---------------------------------------------------
fisherTest[4,]$testResultsColumnName[[1]]

## ---- results = 'asis'---------------------------------------------------
chiSquaredTest <- MulEA::calculateChiSquaredTest(model = modelDfFromFile, sampleVector = experimentalData)
knitr::kable(head(chiSquaredTest, n = 3), caption = "Chi Squared Test Result Data Frame")

## ---- results = 'asis'---------------------------------------------------
chiSquaredTest[4,]$testResultsColumnName[[1]]

## ---- results = 'asis'---------------------------------------------------
hypergeometricTestWithAdjustment <- MulEA::calculateHypergeometricTest(model = modelDfFromFile, sampleVector = experimentalData, adjustMethod = "BH")
knitr::kable(hypergeometricTestWithAdjustment, caption = "Hypergeometric Test Result With Adjustment Data Frame")

