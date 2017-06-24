
# if(!isGeneric("runTest"))
# setGeneric("runTest", function(dataObject, testObject) standardGeneric("runTest"))
setGeneric("runTest", function(testObject) standardGeneric("runTest"))


# setMethod("runTest",
#           signature(testObject = "MuleaHypergeometricTest"),
#           function(testObject) {
#             testObject@test(testObject)
#           })

# setMethod("runTest",
#           signature(dataObject = "muleaData", testObject = "muleaKolmogorovSmirnovTest"),
#           function(dataObject, testObject) {
#
#             testObject@test(dataObject, testObject)
#
#           })
#
# setMethod("runTest",
#           signature(dataObject = "muleaData", testObject = "rankedGseaTest"),
#           function(dataObject, testObject) {
#
#             testObject@test(dataObject, testObject)
#
#           })
#
# setMethod("runTest",
#           signature(dataObject = "muleaData", testObject = "pathNetTest"),
#           function(dataObject, testObject) {
#
#             testObject@test(dataObject, testObject)
#
#           })
#
#
# setMethod("runTest",
#           signature(dataObject = "muleaData", testObject = "muleaFisherTest"),
#           function(dataObject, testObject) {
#
#             testObject@test(dataObject, testObject)
#
#           })
#
# setMethod("runTest",
#           signature(dataObject = "muleaData", testObject = "muleaChiSquaredTest"),
#           function(dataObject, testObject) {
#
#             testObject@test(dataObject, testObject)
#
#           })

