
### UNDER CONSTRUCTION

# cross-validation for 'recosystem'

################### main ftn: xvalReco() ##########################
 
# xvalReco()

# arguments:

#    holdout: number of cases for the test set
#    cls: an R 'parallel' cluster
#    rnk: desired rank for P,Q 
#    nmf: if TRUE, use NMF else SVD

# value: object of class 'xvalreco', consisting mainly of various
# prediction accuracy measures, plus the number of NA predictions

xvalReco <- function(ratingsIn,binaryCase=FALSE,holdout=10000,cls=NULL, 
               rnk=10,nmf=TRUE,printTimes=TRUE)  
{
  require(recosystem)
  testIdxs = sample(1:nrow(ratingsIn),holdout)
  trainSet = ratingsIn[-testIdxs,]
  testSet = ratingsIn[testIdxs,]
  if(is.null(cls)){
     trainTime <- system.time(
        res <- trainReco(trainSet,rnk=rnk,nmf=nmf)
     )
     predTime <- system.time(
        totalPreds <- predict(res,testSet)
     )
  } else {
     trainTime <- system.time(
        res <- trainRecoPar(trainSet,rnk=rnk,nmf=nmf,cls=cls)
     )
     predTime <- system.time(
        totalPreds <- predict(res,testSet,cls)
     )
  }
  if (printTimes) {
     print('training, test times:')
     print(trainTime)
     print(predTime)
  }
  numpredna = sum(is.na(totalPreds))
  result = list(ndata = nrow(ratingsIn),holdout = holdout, 
                numpredna = numpredna)
  # accuracy measures
  class(result) <- 'xvalReco'
  result$idxs <- as.numeric(rownames(testSet))
  result$preds <- totalPreds
  result$actuals <- ratingsIn[result$idxs,3]
  if (!binaryCase) {
     exact <- mean(round(totalPreds) == testSet[,3],na.rm=TRUE)
     mad <- mean(abs(totalPreds-testSet[,3]),na.rm=TRUE)
     rms= sqrt(mean((totalPreds-testSet[,3])^2,na.rm=TRUE))
     # if just guess mean
     meanRat <- mean(testSet[,3],na.rm=TRUE)
     overallexact <- mean(round(meanRat) == testSet[,3],na.rm=TRUE)
     overallmad <- mean(abs(meanRat-testSet[,3]),na.rm=TRUE)
     overallrms <- sd(testSet[,3],na.rm=TRUE)
     result$acc <- list(exact=exact,mad=mad,rms=rms,
           overallexact=overallexact,
           overallmad=overallmad,
           overallrms=overallrms)
  } else {
     # map pred to 0 or 1
     pred <- pmin(totalPreds,1)
     pred <- pmax(pred,0)
     pred <- round(pred)
     exact <- mean(pred == testSet[,3],na.rm=TRUE)
     meanRat <- mean(testSet[,3],na.rm=TRUE)
     overallpred <- if (meanRat >= 0.5) 1 else 0
     overallexact <- 
        mean(round(overallpred) == testSet[,3],na.rm=TRUE)
     result$acc <- list(exact=exact,overallexact=overallexact)
  }
  result
}
 
