
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
  gmtMock <- data.frame(ontologyId = "GO:0000001",
                          ontologyName = "Imagin gen ontology to tests.",
                          listOfValues = I(list(c("a", "b", "c"))),
                          stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "d")
  muleaHypergeometricTestObject <- new("muleaHypergeometricTest", testData = dataFromExperiment)
  expect_warning(hTestRes <- MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject))
  expect_equal(hTestRes$p.value,
               fisher.test(matrix(c(2, 1, 0, 0), 2, 2), alternative = "greater")$p.value)
})

test_that("HypergeometricTest : testData out of pool.", {
  gmtMock <- data.frame(ontologyId = "GO:0000001",
                        ontologyName = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "c")
  poolMock <- c("a", "b", "d")
  muleaHypergeometricTestObject <- new("muleaHypergeometricTest",
                                       testData = dataFromExperiment,
                                       pool = poolMock)
  expect_warning(hTestRes <- MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject))
  expect_equal(hTestRes$p.value,
              fisher.test(matrix(c(2, 0, 0, 1), 2, 2), alternative = "greater")$p.value)
})

test_that("HypergeometricTest : matrix 2,2,2,2.", {
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
  expect_equal(MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject)$p.value,
               fisher.test(matrix(c(2, 2, 2, 2), 2, 2), alternative = "greater")$p.value)
})

test_that("HypergeometricTest : pool >> var + DBi, matrix 2,2,2,18.", {
  gmtMock <- data.frame(ontologyId = "GO:0000001",
                        ontologyName = "Imagin gen ontology to tests.",
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
               fisher.test(matrix(c(2, 2, 2, 18), 2, 2), alternative = "greater")$p.value)
})

test_that("HypergeometricTest : DBi not include pool, matrix 2,0,2,2.", {
  gmtMock <- data.frame(ontologyId = "GO:0000001",
                        ontologyName = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c", "d"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "e", "f")
  poolMock <- c("a", "b", "e", "f", "g", "h")
  muleaHypergeometricTestObject <- new("muleaHypergeometricTest",
                                       testData = dataFromExperiment,
                                       pool = poolMock)
  expect_equal(MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject)$p.value,
               fisher.test(matrix(c(2, 0, 2, 2), 2, 2), alternative = "greater")$p.value)
})

test_that("HypergeometricTest : DB1 + DB2 => pool, matrix 1,3,2,2 and 2,2,1,3.", {
  gmtMock1 <- data.frame(ontologyId = "GO:0000001",
                        ontologyName = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c", "d"))),
                        stringsAsFactors = FALSE)
  gmtMock2 <- data.frame(ontologyId = "GO:0000002",
                        ontologyName = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("e", "f", "g", "h"))),
                        stringsAsFactors = FALSE)
  gmtMock <- rbind(gmtMock1, gmtMock2)

  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("d", "e", "f")
  muleaHypergeometricTestObject <- new("muleaHypergeometricTest",
                                       testData = dataFromExperiment)
  expect_equal(MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject)$p.value,
               c(fisher.test(matrix(c(1, 3, 2, 2), 2, 2), alternative = "greater")$p.value,
                 fisher.test(matrix(c(2, 2, 1, 3), 2, 2), alternative = "greater")$p.value))
})

test_that("HypergeometricTest : DB1 + DB2 => pool, matrix 2,2,2,0 and 2,2,1,3.", {
  gmtMock1 <- data.frame(ontologyId = "GO:0000001",
                         ontologyName = "Imagin gen ontology to tests.",
                         listOfValues = I(list(c("a", "b", "c", "d"))),
                         stringsAsFactors = FALSE)
  gmtMock2 <- data.frame(ontologyId = "GO:0000002",
                         ontologyName = "Imagin gen ontology to tests.",
                         listOfValues = I(list(c("e", "f", "c", "d"))),
                         stringsAsFactors = FALSE)
  gmtMock <- rbind(gmtMock1, gmtMock2)

  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("b", "d", "e", "f")
  muleaHypergeometricTestObject <- new("muleaHypergeometricTest",
                                       testData = dataFromExperiment)
  expect_equal(MulEA::runTest(muleaDataObject, muleaHypergeometricTestObject)$p.value,
               c(fisher.test(matrix(c(2, 2, 2, 0), 2, 2), alternative = "greater")$p.value,
                 fisher.test(matrix(c(3, 1, 1, 1), 2, 2), alternative = "greater")$p.value))
})
