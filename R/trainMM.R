
# TODO:

#    allow covariates for the items

#    accommodate matrices in addition to data frames, in order to all
#    use of bigmemory package

#  modified version of earlier trainMM() etc., May 27, 2017, with
#  new approach to use of covariates X_ijk for user i, item j;
#  NOTE: the covariates will be centered

#  basic model is
#  
#     Y = mu + alpha + beta + eps 
#    
#  for simplicity, the following is at the population level

#  after regressing alpha and/or beta on the vector X of covariates
#  then either alpha or beta or both are replaced by X in the
#  prediction; e.g. if X depends only on the user covariates, then our
#  prediction is
#
#     mu + gamma'V + beta
#
#  where gamma is the vector of regression coefficients; with sampled
#  data rather than population quantities, this means 
#
#     hat(Y_ij) = hat(mu) + hat(gamma)'V_i + hat(beta_j)
#
#  with hat(mu) being the overall mean of the Y data and hat(bet_j)
#  begin Y.j, then mean of Y_ij over all i


#      E(Y_ij | U_ijk = sum_k gamma_k U_ik + sum_k delta_k V_jk

# and the coefficients can be estimated via lm() without a const term

# arguments:

#   ratingsIn: input data, with cols (userID,itemID,rating,
#              covariates); data frame
#   

# value:

#   S3 class of type "ydots", with components:

#      Y..: grand mean, est of mu 
#      Yi.: vector of mean ratings for each user, ests. of alpha_i
#      Y.j: vector of mean ratings for each item, ests. of betaa_j
#      regObj: if have covariates, regression output, e.g. coefs

trainMM <- function(ratingsIn
{
  users <- ratingsIn[,1]
  items <- ratingsIn[,2]
  # IMPORTANT NOTE:
  # user and item IDs may not be consecutive; even if they are
  # consecutive in the original, if we do cross-validation, this 
  # may not be the case; so switch to character IDs
  users <- as.character(users)
  items <- as.character(items)
  ratings <- ratingsIn[,3]
  nms <- names(ratingsIn)
  Y.. <- mean(ratings)  # overall mean
  Yi. <- tapply(ratings,users,mean) # means of all ratings per user
  Y.j <- tapply(ratings,items,mean) # means of all ratings per item
  ydots <- list(grandMean=Y..,usrMeans=Yi.,itmMeans=Y.j)
  haveCovs <- ncol(ratingsIn) > 3
  if (haveCovs) {
     # as noted above, center the covs
     tmp <- scale(ratingsIn[,-(1:3)],scale=FALSE)
     ratingsIn[,-(1:3)] <- tmp
     # need to record the centering process in order to use predict()
     # later, so the mean of each covariate is saved here
     ydots$covmeans <- attr(tmp,'scaled:center')
     # regress ratings against covariates, no constant term
     # NOTE:  could do a weighted least squares, using the Ni, 
     # but since the latter is random too, not needed
     frml <- as.formula(paste(nms[3],'~ .-1'))
     lmout <- lm(frml,data=ratingsIn[,-(1:2)])
     ydots$lmout <- lmout
     Ni <- tapply(ratings,users,length) # number of ratings per user
     ydots$Ni <- Ni
  } 
  class(ydots) = 'ydotsMM'
  invisible(ydots)
}

# predict() method for the 'ydotsMM' class

# in predicting for user i, the code looks at N_i, the number of ratings
# by user i; if that number is below minN, the prediction comes from
# user i's covariate information (if available) instead of from the Yi.
# and Y.j values

# arguments

#    testSet: data frame in same form as ratingsIn above except that there 
#             is no ratings column; thus covariates, if any, are shifted
#             leftward one slot, i.e. userID, itemID, cov1, cov2...
#    ydotsObj: the output of trainMM()
#    minN:  if Ni < minN and have covariates, use the latter instead of
#           Yi. and Y.j; see above

# returns vector of predicted values for testSet
predict.ydotsMM = function(ydotsObj,testSet,minN=0, 
      haveUserCovs=FALSE,haveItemCovs=FALSE,haveBoth=FALSE) 
{
   haveCovs <- ncol(testSet) > 2
   # see comment on as.character() above
   ts1 <- as.character(testSet[,1])  # user IDs, char form
   ts2 <- as.character(testSet[,2])  # item IDs, char form
   # below, pred will basically consist of the user means, except that in the
   # covariate case some will be replaced by predict.lm() values + Y..
   if (!haveCovs) {
      pred <- ydotsObj$usrMeans[ts1] + ydotsObj$itmMeans[ts2] - 
         ydotsObj$grandMean
   }
   else {
      # must center the covariates, using the same centering information
      # used in ydotsObj
      colmeans <- ydotsObj$covmeans
      testSet[,-(1:2)] <- 
         scale(testSet[,-(1:2)],center=colmeans,scale=FALSE)
      pred <- vector(length=nrow)testSet)  # will eventually be output
      names(pred) <- as.character(1:length(pred))
      # which ones to use regression on
      smallNi <- ts1[ydotsObj$Ni[ts1] < minN]
      bigNi <- ts1[ydotsObj$Ni[ts1] < minN]
      # non-regression cases
      pred[bigNi] <- 
         ydotsObj$usrMeans[bigNi] + ydotsObj$itmMeans[bigNi] -
            - ydotsObj$grandMean
      pred[smallNi] <- 
         if (haveBoth) {  # have both user and item covs
            ydotsObj$grandMean + 
               predict(ydotsObj$lmout,testSet[smallNi,-(1:2)])
         } else if (!is.null(userCovs))  # have user covs only
            predict(ydotsObj$lmout,testSet[smallNi,-(1:2)]) +
               ydotsObj$usrMeans[smallNi]
         } else  {  # have item covs only
            predict(ydotsObj$lmout,testSet[smallNi,-(1:2)]) +
               ydotsObj$itmMeans[smallNi]
         }
   }  # end covs section
   pred
}

# test case
checkyd <- function() {
   check <- data.frame(
      userID <- c(1,3,2,1,2),
      itemID <- c(1,1,3,2,3),
      ratings <- 6:10)
   names(check) <- c('u','i','r')
   print(check)
   print(trainMM(check))
   check$cv <- c(1,4,6,2,10)
   names(check)[4] <- 'x'
   print(check)
   cout <- trainMM(check)
   print(cout)
   testset <- check[1:2,-3]
   testset$x <- c(5,8)
   print(predict(cout,testset,2))
}

getDiff <- function(fullMatrix, filledNMF)
{
  indicies <- which(!is.na(fullMatrix), arr.ind = TRUE)
  diff <- matrix(data = NA,nrow = nrow(indicies), 1)
  for(i in 1:nrow(indicies))
    for(j in 1:ncol(indicies)){
      diff[i,1] <- round(fullMatrix[indicies[i,1],indicies[j,2]]) - filledNMF[indicies[i,1],indicies[j,2]]
    }
  
  diff
}


getAcc <- function(fullMatrix,filledNMF, threshold = 0.5) 
{
  diff <- abs(getDiff(fullMatrix, filledNMF))
  avgDiff <- colMeans(abs(diff), na.rm = TRUE, dims =1) # average difference is 1.19 
  # Remove the na's from diff 
  # Calculate the values less than the threshold 
  count = 0;
  tester <- na.exclude(diff)
  for(i in 1:nrow(diff))
  {
    if(diff[i] < 0.5 )
      count = count + 1
  }
  acc <- count/ nrow(diff)
  acc
  
}

buildMatrix <- function(ratingsIn,NAval=NA)
{
  # deal with possible factors
  dmax <- function(d) {
    if (is.factor(d)) return(length(levels(d)))
    max(d)
  }
  users = ratingsIn[,1]
  movies = ratingsIn[,2]
  rating = ratingsIn[,3]
  newMatrix = matrix(NAval, 
                     nrow = dmax(users), ncol = dmax(movies))
  for(rows in 1:nrow(ratingsIn)){
    newMatrix[ratingsIn[rows,1],ratingsIn[rows,2]] = ratingsIn[rows,3]
  }
  return (newMatrix)
}

