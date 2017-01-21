
aa <- data.frame(hTestResults, "lalala" = c("1","2","3","4","5","6","7"))



R_value_obs <- integer(0)
for (i in 1:length(hTestResults$p.value)) {
  R_value_obs[i] <- sum(hTestResults$p.value <= hTestResults$p.value[i])
}

allElements <- unique(unlist(hTestResults$listOfValues))
simulationMatrix <- array(dim = c(length(hTestResults$p.value), steps))
for (j in 1:steps) {
  randomData <- sample(allElements, length(dataFromExperiment))
  for (i in 1:length(hTestResults$p.value)) {


    modelSampleIntersection <- intersect(hTestResults[i, 'listOfValues'][[1]], randomData)
    q <- length(modelSampleIntersection)
    m <- length(hTestResults[i, 'listOfValues'][[1]])
    n <- length(setdiff(allElements, hTestResults[i, 'listOfValues'][[1]]))
    k <- length(randomData)

    # Why 1 - pvalue from test?
    simulationMatrix[i,j] = 1 - phyper(q, m, n, k, lower.tail = FALSE, log.p = FALSE)
  }
}
simulationVector <- as.vector(simulationMatrix)
R_value_exp <- integer(0)
for (l in 1:length(hTestResults$p.value)) {
  # Why is 15 digits?
  # P_Sim_round=round(P_Sim_vec, digits=15)
  R_value_exp[l] <- sum(simulationVector <= hTestResults$p.value[l]) / steps
}

finalDf <- data.frame(hTestResults, "EszterAlg" = R_value_exp / R_value_obs)

