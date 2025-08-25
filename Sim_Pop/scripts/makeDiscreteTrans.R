makeDiscreteTrans <<- function (dataf, stages = NA, discreteTrans = NA, meanToCont = NA, 
          sdToCont = NA, continuousToDiscreteExplanatoryVariables = "size") 
{
  if (is.na(stages[1])) {
    stages <- names(tapply(c(levels(dataf$stage), levels(dataf$stageNext)), 
                           c(levels(dataf$stage), levels(dataf$stageNext)), 
                           length))
    if (!is.na(discreteTrans[1])) 
      stages <- c(stages, dimnames(discreteTrans)[[2]])
  }
  stages <- unique(stages)
  stages <- c(stages[!stages %in% c("continuous", "dead")], 
              "continuous", "dead")
  if (length(stages) == 2) 
    stop("Error - no discrete stages found. If no discrete stages are included in your data file, please specify them in the discreteTrans argument of the makeDiscreteTrans function.")
  if (("number" %in% names(dataf)) == FALSE) 
    dataf$number <- 1
  nDiscreteClasses <- length(stages) - 2
  if (is.na(discreteTrans[1]) & length(discreteTrans) == 1) {
    discreteTrans <- matrix(0, nrow = nDiscreteClasses + 
                              2, ncol = nDiscreteClasses + 1, dimnames = list(stages, 
                                                                              stages[1:(length(stages) - 1)]))
    for (j in stages[1:(length(stages) - 1)]) {
      for (i in stages) discreteTrans[i, j] <- sum(dataf[dataf$stage == 
                                                           j & dataf$stageNext == i, ]$number, na.rm = TRUE)
    }
  }
  if (!"matrix" %in% class(discreteTrans)) 
    stop("Error - the discreteTrans you entered should be a matrix")
  if (nrow(discreteTrans) != length(stages) | ncol(discreteTrans) != 
      (length(stages) - 1)) 
    stop("Error - the discreteTrans matrix you entered should be a square matrix with dimensions equal to the number of stages (including continuous)")
  if (sum(dimnames(discreteTrans)[[1]] == stages) < length(stages)) 
    stop("Error - the row names of your discreteTrans matrix should be in alphabetical order, with continuous being the last one")
  if (sum(dimnames(discreteTrans)[[2]] == stages[1:(length(stages) - 
                                                    1)]) < (length(stages) - 1)) 
    stop("Error - the column names of your discreteTrans matrix should be in alphabetical order, with continuous being the last one")
  for (j in stages[1:(length(stages) - 1)]) discreteTrans[, 
                                                          j] <- discreteTrans[, j]/sum(discreteTrans[, j], na.rm = TRUE)
  if (is.na(meanToCont[1]) & length(meanToCont) == 1) {
    meanToCont <- matrix(NA, nrow = 1, ncol = nDiscreteClasses, 
                         dimnames = list(1, stages[1:nDiscreteClasses]))
    for (j in stages[which(as.numeric(discreteTrans["continuous", 
                                                    1:nDiscreteClasses]) > 0)]) {
      meanToCont[, j] <- mean(dataf[dataf$stage == j & 
                                      dataf$stageNext == "continuous", ]$sizeNext, 
                              na.rm = TRUE)
    }
  }
  if (!"matrix" %in% class(meanToCont)) 
    stop("Error - the meanToCont matrix you entered should be a matrix")
  if (nrow(meanToCont) != 1) 
    stop("Error - the meanToCont matrix you entered should contain just 1 row with means (or NA's for those discrete stages from which no individuals move to the continuous class")
  if (sum(dimnames(meanToCont)[[2]] == stages[1:nDiscreteClasses]) < 
      nDiscreteClasses) 
    stop("Error - the column names of the meanToCont matrix you entered should be in alphabetical order and match the column names of the discrete classes in discreteTrans (so without continuous). If some of the discete stages are not mentioned in your data file, this error can be fixed by adding those stages first: levels(dataf$stage)<-c(levels(dataf$stage),<unmentioned_discrete_stages>)")
  if (is.na(sdToCont[1]) & length(sdToCont) == 1) {
    sdToCont <- matrix(NA, nrow = 1, ncol = nDiscreteClasses, 
                       dimnames = list(1, stages[1:nDiscreteClasses]))
    for (j in stages[which(as.numeric(discreteTrans["continuous", 
                                                    1:nDiscreteClasses]) > 0)]) {
      sdToCont[, j] <- sd(dataf[dataf$stage == j & dataf$stageNext == 
                                  "continuous", ]$sizeNext, na.rm = TRUE)
    }
  }
  if (!"matrix" %in% class(sdToCont)) 
    stop("Error - the sdToCont matrix you entered should be a matrix")
  if (nrow(sdToCont) != 1) 
    stop("Error - the sdToCont matrix you entered should contain just 1 row with means (or NA's for those discrete stages from which no individuals move to the continuous class")
  if (sum(dimnames(sdToCont)[[2]] == stages[1:nDiscreteClasses]) < 
      nDiscreteClasses) 
    stop("Error - the column names of the sdToCont matrix you entered should be in alphabetical order and match the column names of the discrete classes in discreteTrans (so without continuous). If some of the discete stages are not mentioned in your data file, this error can be fixed by adding those stages first: levels(dataf$stage)<-c(levels(dataf$stage),<unmentioned_discrete_stages>)")
  if (sum(discreteTrans[stages[1:nDiscreteClasses], "continuous"]) == 
      0) {
    moveToDiscrete <- glm(rep(0, 21) ~ 1, family = binomial)
  }
  else {
    subData <- subset(dataf, dataf$stage == "continuous" & 
                        dataf$surv == 1)
    subData$contToDiscrete <- 1
    subData$contToDiscrete[subData$stageNext == "continuous"] <- 0
    subData$size2 <- subData$size^2
    subData$size3 <- subData$size^3
    if (length(grep("expsize", as.character(continuousToDiscreteExplanatoryVariables))) > 
        0) 
      subData$expsize <- exp(subData$size)
    if (length(grep("logsize", as.character(continuousToDiscreteExplanatoryVariables))) > 
        0) 
      subData$logsize <- log(subData$size)
    moveToDiscrete <- glm(paste("contToDiscrete~", continuousToDiscreteExplanatoryVariables, 
                                sep = ""), family = binomial, data = subData)
  }
  disTrans <- new("discreteTrans")
  disTrans@discreteTrans <- discreteTrans
  disTrans@meanToCont <- meanToCont
  disTrans@sdToCont <- sdToCont
  disTrans@moveToDiscrete <- moveToDiscrete
  return(disTrans)
}
