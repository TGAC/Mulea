
library(MulEA)
context("HypergeometricTest")

test_that("HypergeometricTest : object creation test without pool.", {
  dataFromExperiment <- c("a", "b", "c")
  muleaHypergeometricTestObject <- new("muleaHypergeometricTest", testData = dataFromExperiment)
  expect_equal(muleaHypergeometricTestObject@testData, c("a", "b", "c"))
  expect_equal(muleaHypergeometricTestObject@pool, character(0))
})

test_that("HypergeometricTest : object creation test with pool.", {
  dataFromExperiment <- c("a", "b", "c")
  poolData <- c("a", "c", "d")
  muleaHypergeometricTestObject <- new("muleaHypergeometricTest",
                                       testData = dataFromExperiment,
                                       pool = poolData)
  expect_equal(muleaHypergeometricTestObject@testData, c("a", "b", "c"))
  expect_equal(muleaHypergeometricTestObject@pool, c("a", "c", "d"))
})

test_that("HypergeometricTest : testData out of DB model.", {
  gmtMock <- data.frame(category = "GO:0000001",
                          description = "Imagin gen ontology to tests.",
                          listOfValues = I(list(c("a", "b", "c"))),
                          stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "d")
  muleaHypergeometricTestObject <- new("muleaHypergeometricTest", testData = dataFromExperiment)
  expect_warning(hTestRes <- MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject))
})

test_that("HypergeometricTest : testData out of DB pool.", {
  gmtMock <- data.frame(category = "GO:0000001",
                        description = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "c")
  poolMock <- c("a", "b", "d")
  muleaHypergeometricTestObject <- new("muleaHypergeometricTest",
                                       testData = dataFromExperiment,
                                       pool = poolMock)
  expect_warning(hTestRes <- MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject))
})

test_that("HypergeometricTest : matrix 2,2,2,2.", {
  gmtMock <- data.frame(category = "GO:0000001",
                        description = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c", "d"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "e", "f")
  poolMock <- c("a", "b", "c", "d", "e", "f", "g", "h")
  muleaHypergeometricTestObject <- new("muleaHypergeometricTest",
                                       testData = dataFromExperiment,
                                       pool = poolMock)
  expect_equal(MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject)$p.value,
               fisher.test(matrix(c(2, 2, 2, 2), 2, 2), alternative = "less")$p.value)
})

test_that("HypergeometricTest : pool >> var + DBi, matrix 2,2,2,2.", {
  gmtMock <- data.frame(category = "GO:0000001",
                        description = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c", "d"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "e", "f")
  poolMock <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p",
                "q", "r", "s", "t", "u", "w", "x", "y")
  muleaHypergeometricTestObject <- new("muleaHypergeometricTest",
                                       testData = dataFromExperiment,
                                       pool = poolMock)
  expect_equal(MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject)$p.value,
               fisher.test(matrix(c(2, 2, 2, 18), 2, 2), alternative = "less")$p.value)
})

