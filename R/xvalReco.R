
### UNDER CONSTRUCTION

# cross-validation for 'recosystem'

################### partiitioning routines ##########################

# getTrainSet():

# arguments:
#    ratingsIn: the usual raw input matrix, usrID, itmID, rating cols 
#    trainprop: probability that a row from ratingsIn is selected for
#               the training set

# value:
#    training set, in the format of ratingsIn, plus a component
#    trainidxs, the indices of the training set in the original data

getTrainSet <- function(ratingsIn,trainprop = 0.5)
{
   rownew = nrow(ratingsIn)
   trainRow = floor(trainprop*rownew)
   trainidxs = sample(1:rownew,trainRow)
   trainSet = ratingsIn[trainidxs,]
   trainSet$trainidxs = trainidxs
   trainSet
} 

# getTestSet():

# returns the set-theoretic complement of the training set, to be used
# as the test set

getTestSet <- function(ratingsIn, trainSet)
{
   ratingsIn[-trainSet$trainidxs,]
}

################### main ftn: xvalReco() ##########################
 
# xvalReco()

# arguments:

#    trainprop: as above
#    cls: an R 'parallel' cluster
#    rnk: rank of P,Q 

# value: object of class 'xvalreco', consisting mainly of various
# prediction accuracy measures, plus the number of NA predictions

xvalReco <- function(ratingsIn, trainprop = 0.5,
                     cls = NULL,
                     rnk = 10)  
{
  require(recosystem)
    trainSet = getTrainSet(ratingsIn, trainprop)
    testSet = getTestSet(ratingsIn, trainSet)
  if(is.null(cls)){
    res = trainReco(trainSet,rnk)
    totalPreds = predict(res,testSet)
  } else {
       # res <- trainRecoPar(ratingsIn,rnk,cls) {
##    clusterEvalQ(cls,library(partools))
##    distribsplit(cls, 'ratingsIn')
##    clusterEvalQ(cls,library(rectools))
##    clusterEvalQ(cls, trainSet <- getTrainSet(ratingsIn))
##    testSet = clusterEvalQ(cls, testSet< - getTestSet(ratingsIn,trainSet))
##    testSet = mapply(c,testSet$ratings[1],testSet$ratings[2],SIMPLIFY = FALSE)
##    clusterEvalQ(cls,resu <- trainReco(trainSet,rnk=10))
##    allPreds = clusterEvalQ(cls, pred <- predict(ratingsIn,testSet))
    res <- trainRecoPar(trainSet,rnk,cls)
    totalPreds <- predict(res,testSet,cls)
  }
  numpredna = sum(is.na(totalPreds))
  result = list(ndata = nrow(ratingsIn),trainprop = trainprop, 
                numpredna = numpredna)
  # accuracy measures
  exact <- mean(round(totalPreds) == testSet[,3],na.rm=TRUE)
  mad <- mean(abs(totalPreds-testSet[,3]),na.rm=TRUE)
  rms= sqrt(mean((totalPreds-testSet[,3])^2,na.rm=TRUE))
  # if just guess mean
  meanRat <- mean(testSet[,3],na.rm=TRUE)
  overallexact <-
     mean(round(meanRat) == testSet[,3],na.rm=TRUE)
  overallmad <- mean(abs(meanRat-testSet[,3]),na.rm=TRUE)
  overallrms <- sd(testSet[,3],na.rm=TRUE)
  result$acc <- list(exact=exact,mad=mad,rms=rms,
        overallexact=overallexact,
     overallmad=overallmad,
     overallrms=overallrms)
  if (is.null(cls)) {
     result$idxs <- as.numeric(rownames(testSet))
     result$preds <- totalPreds
     result$actuals <- ratingsIn[result$idxs,3]
  }
  class(result) <- 'xvalb'
  result
}
 
