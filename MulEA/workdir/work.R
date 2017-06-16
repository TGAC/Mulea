
muleaPkgDir <- find.package("MulEA")
modelDfFromFile <- MulEA::readGmtFileAsDF(gmtFilePath = paste(muleaPkgDir,"/example/model.gmt", sep = ""))
muleaDataObject <- new(Class = "muleaData", gmt = modelDfFromFile)
dataFromExperimentF <- c("FBgn0004407", "FBgn0010438", "FBgn0003742", "FBgn0029709", "FBgn0030341", "FBgn0037044", "FBgn0002887", "FBgn0028434", "FBgn0030170", "FBgn0263831")
muleaKolmogorovSmirnovTestObject <- new("muleaKolmogorovSmirnovTest", testData = dataFromExperimentF)
ksTestRes <- MulEA::runTest(muleaDataObject, muleaKolmogorovSmirnovTestObject)


N <- length(muleaKolmogorovSmirnovTestObject@testData)
categoryValues <- muleaDataObject@gmt$listOfValues[[6]]
# Cut model.


match(c("q","w","a"),c("q","w"))
categoryValuesMatchedToPool <- match(muleaKolmogorovSmirnovTestObject@testData, categoryValues)
categoryValuesMatchedToPool <- categoryValuesMatchedToPool[!is.na(categoryValuesMatchedToPool)]
categoryValues <- categoryValues[categoryValuesMatchedToPool]

a <- match(categoryValues, muleaKolmogorovSmirnovTestObject@testData)
# a <- x.a[!is.na(x.a)]
# a <- a[!is.na(a)]
# if ( length(a) == 0 ) {
#   return(1)
# }
# return(ks.test(a, seq_len(N)[-a], alternative = "greater")$p.value)
ks.test(a, seq_len(N))$p.value

distributionA <- c("a","b","c","e","g","h")
distributionB <- c("c","e","g","i","j","k")

distributionANoRepresentation <- c(1,2,3,4,6,7,8)
distributionBNoRepresentation <- c(2,3,4,6,7,8)

ks.test(distributionANoRepresentation, distributionBNoRepresentation)

x <- rnorm(50)
y <- runif(30)
# Do x and y come from the same distribution?
x <- c(1,2,3,4,5)
y <- c(1,2,2,3,4,5)
ks.test(x, y)
ks.test(c("A","B"),c("A","B"))



gmtMock <- data.frame(ontologyId = "GO:0000001",
                      ontologyName = "Imagin gen ontology to tests.",
                      listOfValues = I(list(c("a","b","c","e","g","h"))),
                      stringsAsFactors = FALSE)
muleaDataObject <- new(Class = "muleaData", gmt = gmtMock)
dataFromExperiment <- c("c","e","g","i","j","k")
muleaKolmogorovSmirnovTestObject <- new("muleaKolmogorovSmirnovTest", testData = dataFromExperiment)
ksTestRes <- MulEA::runTest(muleaDataObject, muleaKolmogorovSmirnovTestObject)
ks.test(distributionANoRepresentation, distributionBNoRepresentation)



x1 <- rnorm(5000)
y1 <- rnorm(5000)
y2 <- runif(5000)
ks.test(x1, y1)
ks.test(x1, y2)


ks.test(c(1,2,3),c(1,2,4))
ks.test(c(2,3,4),c(1,2,4))
ks.test(c(2,4,3),c(2,4,1))

ks.test(c(1,2,3,5),c(1,2,4))
ks.test(c(2,3,4,5),c(1,2,4))
ks.test(c(2,4,3,5),c(2,4,1))

ks.test(c(1,2,3,4,5,7,6,8,9),c(1,2,4,5))
ks.test(c(5,2,9,4,1,6,7,8,3),c(1,2,4,5))


toSort <- c("W","a","w","X","1","1a","as","X","A","23e")
afterSort <- sort(toSort)
duplicated(afterSort)





