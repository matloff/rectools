
### UNDER CONSTRUCTION

# cross-validation for 'recosystem'

################### main ftn: xvalReco() ##########################
 
# xvalReco()

# arguments:

#    trainprop: proportion of data to allocate to training set
#    cls: an R 'parallel' cluster
#    rnk: desired rank for P,Q 
#    nmf: if TRUE, use NMF else SVD

# value: object of class 'xvalreco', consisting mainly of various
# prediction accuracy measures, plus the number of NA predictions

xvalReco <- function(ratingsIn, trainprop=0.8,cls=NULL, 
               rnk=10,nmf=FALSE)  
{
    require(recosystem)
    trainSet = getTrainSet(ratingsIn, trainprop)
    testSet = getTestSet(ratingsIn, trainSet)
  if(is.null(cls)){
    res = trainReco(trainSet,rnk=rnk,nmf=nmf)
    totalPreds = predict(res,testSet)
  } else {
    res <- trainRecoPar(trainSet,rnk=rnk,nmf=nmf,cls=cls)
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
        result$idxs <- as.numeric(rownames(testSet))
        result$preds <- totalPreds
        result$actuals <- ratingsIn[result$idxs,3]
  class(result) <- 'xvalReco'
  result
}
 
