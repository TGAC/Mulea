
library(MulEA)
context("KolmogorovSmirnovTest")

test_that("KolmogorovSmirnovTest : object creation test (pool is testData in this test).", {
  dataFromExperiment <- c("a", "b", "c")
  muleaKolmogorovSmirnovTestObject <- new("muleaKolmogorovSmirnovTest", testData = dataFromExperiment)
  expect_equal(muleaKolmogorovSmirnovTestObject@testData, c("a", "b", "c"))
})

test_that("KolmogorovSmirnovTest : DB model partialy out of testData.", {
  gmtMock <- data.frame(ontologyId = "GO:0000001",
                        ontologyName = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "d")
  muleaKolmogorovSmirnovTestObject <- new("muleaKolmogorovSmirnovTest", testData = dataFromExperiment)
  expect_warning(ksTestRes <- MulEA::runTest(muleaDataObject, muleaKolmogorovSmirnovTestObject))
  expect_equal(ksTestRes$p.value, ks.test(c(1, 2), seq_len(length(muleaKolmogorovSmirnovTestObject@testData)))$p.value)
})

test_that("KolmogorovSmirnovTest : empty intersection of experiment data with category value category value.", {
  gmtMock <- data.frame(ontologyId = "GO:0000001",
                        ontologyName = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("d", "e", "f", "g")
  muleaKolmogorovSmirnovTestObject <- new("muleaKolmogorovSmirnovTest", testData = dataFromExperiment)
  ksTestRes <- MulEA::runTest(muleaDataObject, muleaKolmogorovSmirnovTestObject)
  expect_equal(ksTestRes$p.value, 1)
})

test_that("KolmogorovSmirnovTest : DB model fully inside testData.", {
  gmtMock <- data.frame(ontologyId = "GO:0000001",
                        ontologyName = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("c", "f", "j"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "c", "d", "e", "f", "g", "h", "j")
  muleaKolmogorovSmirnovTestObject <- new("muleaKolmogorovSmirnovTest", testData = dataFromExperiment)
  expect_warning(ksTestRes <- MulEA::runTest(muleaDataObject, muleaKolmogorovSmirnovTestObject))
  expect_equal(ksTestRes$p.value, ks.test(c(3, 6, 9), seq_len(length(muleaKolmogorovSmirnovTestObject@testData)))$p.value)
})

test_that("KolmogorovSmirnovTest : DB model the same as testData.", {
  gmtMock <- data.frame(ontologyId = "GO:0000001",
                        ontologyName = "Imagin gen ontology to tests.",
                        listOfValues = I(list(c("a", "b", "c", "g", "h", "j"))),
                        stringsAsFactors = FALSE)
  muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
  dataFromExperiment <- c("a", "b", "c", "g", "h", "j")
  muleaKolmogorovSmirnovTestObject <- new("muleaKolmogorovSmirnovTest", testData = dataFromExperiment)
  expect_warning(ksTestRes <- MulEA::runTest(muleaDataObject, muleaKolmogorovSmirnovTestObject))
  expect_equal(ksTestRes$p.value, ks.test(c(1, 2, 3, 4, 5, 6), seq_len(length(muleaKolmogorovSmirnovTestObject@testData)))$p.value)
})
