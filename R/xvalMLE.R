
# splits input data into training and test sets, fits "lme4" model on
# the former, then predicts the latter

# arguments:

#   ratingsIn: input data, with first cols (userID,itemID,rating,
#              covariates)
#   trainprop: proportion of data for the training set
#   cls: if non-null, do this in parallel

# in the parallel case, use 'partools' philosophy of Leave It There;
# see comments in trainMLE.R

# value:

#    accuracy value

xvalMLE <- function(ratingsIn, trainprop=0.8,cls=NULL){
  ratIn = ratingsIn 
  # split into random training and validation sets 
  nrowRatIn = nrow(ratIn)
  rowNum = floor(trainprop * nrowRatIn)
  trainIdxs = sample(1:nrowRatIn,rowNum)
  trainingSet = ratIn[trainIdxs, ]
  MLEobj = trainMLE(trainingSet,cls)
  testIdxs = setdiff(1:nrowRatIn,trainIdxs)
  testSet = ratIn[testIdxs,]
  # allow.new.levels = TRUE means predict() won't bomb if new u_i or i_j
  # are encountered in test set not in the training set, as is likely
  testSet$pred =  
     if(is.null(cls)) {
        predict(MLEobj,testSet[,-3],allow.new.levels=TRUE)
     } else
        predict(MLEobj,testSet[,-3],allow.new.levels=TRUE,cls=cls)
  numpredna = sum(is.na(testSet$pred))
  # calculate accuracy 
  result = list(ndata=nrowRatIn,trainprop=trainprop,numpredna=numpredna)
  # accuracy measures
  exact <- mean(round(testSet$pred) == testSet[,3],na.rm=TRUE)
  mad <- mean(abs(testSet$pred-testSet[,3]),na.rm=TRUE)
  rms= sqrt(mean((testSet$pred-testSet[,3])^2,na.rm=TRUE))
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
  result$idxs <- testIdxs
  result$preds <- testSet$pred
  result$actuals <- testSet[,3]
  class(result) <- 'xvalb'
  result
}

# check
checkxv <- function(trainprop=0.5,acc='mad') {
   check <- 
      data.frame(userID = c(1,3,2,1,2,3),itemID = c(1,1,3,2,3,3),ratings=5:10)
   print(check)
   print(xvalMLE(check,trainprop,acc))
   check$cv = c(1,2,8,6,3,3)
   print(check)
   print(xvalMLE(check,trainprop,acc))
}


