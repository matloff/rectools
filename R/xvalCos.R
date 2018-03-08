
# splits input data into training and test sets, applies
# findUsrItmData.R to the former, then predicts the latter

# arguments:

#   ratingsIn: input data, with first cols (userID,itemID,rating,
#              covariates); data frame, unless cls is non-null, in which
#              case this argument is the quoted name of the distributed 
#              data frame
#   trainprop: proportion of data for the training set
#   k: number of nearest neighbors
#   wtcovs: as in predict.usrData
#   wtcats: as in predict.usrData

# value:

#    accuracy value

## IMPORTANT NOTE: see note about character-based userIDs in findUserData.R

xvalCos <- function(ratingsIn,k,usrCovs=NULL,itmCats=NULL,
   wtcovs=NULL,wtcats=NULL,
   trainprop=0.8)
{
   # split into random training and validation sets 
   nrowRatIn = nrow(ratingsIn)
   numRows = floor(trainprop * nrowRatIn)
   trainIdxs = sample(1:nrowRatIn,numRows)
   print(trainIdxs)
   trainingSet = ratingsIn[trainIdxs, ,drop=FALSE]
   print(trainingSet)
   trainRatings = trainingSet[,3]
   trainItems = trainingSet[,2]
   trainUsers = trainingSet[,1]
   testIdxs <- setdiff(1:nrowRatIn,trainIdxs)
   testSet = ratingsIn[testIdxs,,drop=FALSE]
   # now set up training set for cosine analysis
   trainData <- formUserData(trainingSet,usrCovs,itmCats)
   # for each user i in the test data, find the items rated by user i in
   # the test data, then "predict" them (ignoring the known values)
   testData <- formUserData(testSet,usrCovs,itmCats)
   preds <- c(NULL,NULL)  # row i will be (predicted value, actual value)
   for (l in 1:length(testData)) {
      oneNewDatum <- testData[[l]]
      # now predict each of this new user's ratings, pretending
      # momentarily that the user hadn't rated the given item
      newUserItems <- oneNewDatum$itms
      nNewUserItems <- length(newUserItems)
      # but if this new user doesn't have any information usable for
      # nearest-neighbor computation, then skip him/her 
      if (nNewUserItems > 1 || !is.null(usrCovs) || !is.null(itmCats)) {
         for (j in 1:nNewUserItems) {
            pretendNewUser <- oneNewDatum
            pretendNewUser$ratings <- pretendNewUser$ratings[-j]
            pretendNewUser$itms <- pretendNewUser$itms[-j]
            predVal <- 
               predict(trainData,pretendNewUser,newUserItems[j],k)
            actualVal <- oneNewDatum$ratings[j]
            preds <- rbind(preds,c(predVal,actualVal))
         }
      }
   }
  numpredna = sum(is.na(preds[,1])) 
  # calculate accuracy 
  result = list(ndata=nrowRatIn,trainprop=trainprop,numpredna=numpredna)
  roundpreds = round(preds[,1])
  exact = mean(preds[,1] == preds[,2],na.rm=TRUE)
  mad = mean(abs(preds[,1] - preds[,2]),na.rm=TRUE)
  rms = sqrt(mean((preds[,1] - preds[,2])^2,na.rm=TRUE))
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
   print(xvalMM(check,trainprop,acc))
   check$cv = c(1,2,8,6,3,3)  # covariate
   print(check)
   print(xvalMM(check,trainprop,acc))
}

testXvalCos <- function() {
  set.seed(9999)
  rts <- data.frame(matrix(ncol = 3, nrow = 0))
  rts <- rbind(rts, c(1,1,5), c(1,2,4), c(1,3,6), c(1,4,4), c(1,6,2),
               c(2,1,3), c(2,2,6), c(2,3,1), c(2,4,5), c(1,5,3),
               c(3,1,4), c(3,2,3), c(3,5,4), c(3,6,3), c(3,7,2),
               c(4,1,6), c(4,3,2), c(4,4,5), c(4,5,3), c(4,7,3),
               c(5,1,3), c(5,2,6), c(5,3,2), c(5,5,4), c(5,6,1))
  print(xvalCos(rts, 1)$acc)
}

