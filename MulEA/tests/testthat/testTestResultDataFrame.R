
library(MulEA)
context("TestResultDataFrame.")

test_that("TestResultDataFrame : hypergeometric gives properly formated result.", {
  gmtMock <- data.frame(ontologyId = "GO:0000001",
                        ontologyName = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c", "d"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "e", "f")
  poolMock <- c("a", "b", "c", "d", "e", "f", "g", "h")
  muleaHypergeometricTestObject <- new("muleaHypergeometricTest",
                                       testData = dataFromExperiment,
                                       pool = poolMock)
  expect_equal(colnames(MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject)),
               c("ontologyId", "ontologyName", "listOfValues", "overlappingData", "contingencyTable", "p.value"))
})

test_that("TestResultDataFrame : fisher gives properly formated result.", {
  gmtMock <- data.frame(ontologyId = "GO:0000001",
                        ontologyName = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c", "d"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "e", "f")
  poolMock <- c("a", "b", "c", "d", "e", "f", "g", "h")
  muleaHypergeometricTestObject <- new("muleaFisherTest",
                                       testData = dataFromExperiment,
                                       pool = poolMock)
  expect_equal(colnames(MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject)),
               c("ontologyId", "ontologyName", "listOfValues", "overlappingData", "contingencyTable", "p.value"))
})

test_that("TestResultDataFrame : chiSquared gives properly formated result.", {
  gmtMock <- data.frame(ontologyId = "GO:0000001",
                        ontologyName = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c", "d"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b")
  muleaHypergeometricTestObject <- new("muleaChiSquaredTest",
                                       testData = dataFromExperiment)
  expect_equal(colnames(MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject)),
               c("ontologyId", "ontologyName", "listOfValues", "overlappingData", "contingencyTable", "p.value"))
})

test_that("TestResultDataFrame : KS gives properly formated result.", {
  gmtMock <- data.frame(ontologyId = "GO:0000001",
                        ontologyName = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c", "d"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "e", "f")

  muleaHypergeometricTestObject <- new("muleaKolmogorovSmirnovTest",
                                       testData = dataFromExperiment)

  expect_equal(colnames(MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject)),
               c("ontologyId", "ontologyName", "listOfValues", "p.value"))
})

test_that("TestResultDataFrame : SeaSubranian gives properly formated result.", {
  gmtMock <- data.frame(ontologyId = "GO:0000001",
                        ontologyName = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c", "d"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "e", "f")
  dataFromExperimentScores <- c(0.09, 0.11, 0.15, 0.20)

  muleaHypergeometricTestObject <- new("rankedGseaTest",
                                       testData = dataFromExperiment,
                                       scores = dataFromExperimentScores, p = 3, numberOfPermutations = 10000)

  expect_equal(colnames(MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject)),
               c("ontologyId", "ontologyName", "listOfValues", "p.value"))
})
