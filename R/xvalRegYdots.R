
# value:

#    accuracy values (MAE, RMS etc.)

xvalRegYdots <- function(ratingsIn,regModel='lm',rmArgs=NULL,
                   holdout=10000,printTimes=TRUE)
{
  ratIn = ratingsIn 
 
  # split into random training and validation sets 
  nrowRatIn = nrow(ratIn)
  # training stage
  testIdxs = sample(1:nrowRatIn,holdout)
  trainSet = ratIn[-testIdxs, ]
  trainRatings = trainSet[,3]
  trainItems = trainSet[,2]
  trainUsers = trainSet[,1]
  testA = ratIn[testIdxs,]
  tmp <- system.time(
     ryout <- trainRegYdots(trainSet,regModel=regModel,rmArgs=rmArgs) 
  )
  if (printTimes) cat('training time: ',tmp,'\n')
  # test stage
  # delete users or items are in the test set but not the training set
  deleted <- deleteNewIDsAlt(testA,trainUsers,trainItems)  
  nondeleted <- setdiff(row.names(testA),deleted)
  testA <- testA[nondeleted,]
  # get (user means, item means, uN, iN) data
  browser()
  trainUINN <- ryout$UINN
  # need to convert test data to Ydots form
  x <- convertX(testA,trainUINN)
  if (regModel == 'dn') x <- scale(x)
  rns <- row.names(testA)
  testA <- cbind(x,ratIn[nondeleted,3])
  row.names(testA) <- rns
  names(testA)[ncol(testA)] <- 'rats'
  tmp <- system.time(
     pred <- predict(ryout,testA[,-ncol(testA)]) 
  )
  if (printTimes) cat('validation time: ',tmp,'\n')
  # calculate accuracy 
  result = list(nFullData=nrowRatIn,holdout=holdout,preds=pred,
     deleted=deleted)
  # accuracy measures
  exact <- mean(round(pred) == testA[,3],na.rm=TRUE)
  mad <- mean(abs(pred-testA[,3]),na.rm=TRUE)
  rms= sqrt(mean((pred-testA[,3])^2,na.rm=TRUE))
  # if just guess mean
  meanRat <- mean(testA[,3],na.rm=TRUE)
  overallexact <- 
     mean(round(meanRat) == testA[,3],na.rm=TRUE)
  overallmad <- mean(abs(meanRat-testA[,3]),na.rm=TRUE)
  overallrms <- sd(testA[,3],na.rm=TRUE)  
  result$acc <- list(exact=exact,mad=mad,rms=rms,
     overallexact=overallexact,
     overallmad=overallmad,
     overallrms=overallrms)
  result$idxs <- testIdxs
  result$preds <- pred
  result$actuals <- testA[,3]
  result$type <- 'dn'
  class(result) <- 'xvalb'
  result
}

# any users or items in test set but not the training set?
deleteNewIDsAlt <- function(testSet,trainUsers,trainItems)
{
   deleted <- NULL  # named row numbers from the original full data
   rns <- row.names(testSet)
   tmp <- setdiff(unique(testSet[,1]),unique(trainUsers))
   if (length(tmp) > 0) {
      for (usr in tmp) {
         tmp1 <- which(testSet[,1] == usr)
         # tmp1 is ordinal row numbers within testSet; the latter may
         # have shrunken in earlier iterations!
         deleted <- c(deleted,row.names(testSet[tmp1,]))
      }
   }
   tmp <- setdiff(unique(testSet[,2]),unique(trainItems))
   if (length(tmp) > 0) {
      for (itm in tmp) {
         tmp1 <- which(testSet[,2] == itm)
         # tmp1 is ordinal row numbers within testSet; the latter may
         # have shrunken in earlier iterations, here or above!
         deleted <- c(deleted,row.names(testSet[tmp1,]))
         testSet <- testSet[-tmp1,]
      }
   }
   unique(deleted)
}

# check
checkxv <- function() {
   set.seed(999999)
   check <- data.frame(
      u=sample(1:5,12,replace=TRUE),
      i=sample(11:15,12,replace=TRUE),
      r=sample(21:25,12,replace=TRUE))
   print(check) 
   xvout <- xvalMM(check,0.5)
   print(xvout$idxs)  # 1 6 7 8 11 12
   print(xvout$deleted)  # "8" "11"
   print(xvout$preds)
   print(xvout$actuals)
   check$cv = sample(31:35,12,replace=TRUE)  # covariate
   print(check)
   print(xvout)
}

# see how well covs do on small Ni users
xvSmallNi <- function(ratIn,maxN,minN) {
   ri1 <- as.character(ratIn[,1])
   NiVals <- tapply(ri1,ri1,length)
   smallNi <- which(NiVals <= maxN)
   rows <- as.numeric(names(smallNi))
   smallRatIn <- ratIn[rows,]
   xvalMM(smallRatIn)$acc
}
