
# Method of Moments approach

# TODO:

#    accommodate matrices in addition to data frames, in order to all
#    use of bigmemory package
#    
#  for simplicity, the following discussion is at the population level

#  basic model is
#  
#     Y = mu + alpha + beta + eps 
#
# with alpha, beta and eps being independent and having mean 0

#  after regressing Y on the vector X of covariates then the model
#  becomes
#  
#     Y = gamma'X  + alpha + beta + eps 
#
#  with X including a 1 component 

# on the sample level, define:
# 
# Y.. = overall sample mean 
# Yi. = sample mean of the Y_ij over j for fixed i
# Y.j = sample mean of the Y_ij over i for fixed j
# Xi. = sample mean of the X_ij over j for fixed i
# X.j = sample mean of the X_ij over i for fixed j
# 
# then set gammahat = result of regressing Y on X thoughout the sample
# 
# since E(beta) = 0, Yi. should be approximatley gammahat'Xi. + alpha_i, so set
# 
# alphahat_i = Yi. - gammahat' Xi.
# 
# and similarly
# 
# betahat_j = Y.j - gammahat' X.j

# the predicted value for Y_ij is then

# gammahat' X_ij + alphahat_i + betahat_j

#########################  trainMM()  ##############################

# arguments:

#   ratingsIn: input data, with cols (userID,itemID,rating,
#              covariates)

# value:

#   S3 class of type "ydots", with components:

#      Y..: grand mean, est of mu 
#      Yi.: vector of mean ratings for each user, ests. of alpha_i
#      Y.j: vector of mean ratings for each item, ests. of betaa_j
#      lmout: object returned by running the regression analysis 
#      Ni.: vector of number of ratings by each user
#      N.j: vector of number of ratings of each item

trainMM <- function(ratingsIn,userCovsStartCol=NULL,itemCovsStartCol=NULL)
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
  n <- nrow(ratingsIn)
  userRows <- split(1:n,users)
  itemRows <- split(1:n,items)
  Y.. <- mean(ratings)  # overall mean

  uimean <- function(uirow) mean(ratings[uirow)
  Yi. <- sapply(userRows,uimean)
  Y.j <- sapply(itemRows,uimean)

  Yi. <- tapply(ratings,users,mean) # means of all ratings per user
  Y.j <- tapply(ratings,items,mean) # means of all ratings per item
  ydots <- list(grandMean=Y..,usrMeans=Yi.,itmMeans=Y.j)
  ydots$trainingUsers <- unique(users)
  ydots$trainingItems <- unique(items)
  haveCovs <- ncol(ratingsIn) > 3
  if (haveCovs) {
     covCols <- getCovCols(userCovsStartCol,itemCovsStartCol,ncol(ratingsIn))
     usrCovCols <- covCols[[1]]
     itmCovCols <- covCols[[2]]
     ydots$usrCovCols <- usrCovCols  # vector of column numbers
     ydots$itmCovCols <- itmCovCols  # vector of column numbers
     # as noted above, center the covs
     tmp <- scale(ratingsIn[,-(1:3)],scale=FALSE)
     ratingsIn[,-(1:3)] <- tmp
     # need to record the centering process in order to use predict()
     # later, so the mean of each covariate is saved here
     ydots$covMeans <- attr(tmp,'scaled:center')
     # regress ratings against covariates, no constant term
     # NOTE:  could do a weighted least squares, using the Ni, 
     # but treating the latter is random too, not needed;
     # user covs first, if any
     if (!is.null(usrCovCols)) {
        nms3 <- nms[3]
        cmd <- paste(nms3, ' - mean(',nms3,') ~ .-1',sep='')
        frml <- as.formula(cmd)
        frml <- as.formula(paste(nms3,'-~ .-1'))
        lmout <- lm(frml,data=ratingsIn[,c(3,usrCovCols)])
        ydots$lmoutUsr <- lmout
     }
     # now item covs, if any
     if (!is.null(itmCovCols)) {
        nms3 <- nms[3]
        cmd <- paste(nms3, ' - mean(',nms3,') ~ .-1',sep='')
        frml <- as.formula(cmd)
        lmout <- lm(frml,data=ratingsIn[,c(3,itmCovCols)])
        ydots$lmoutItm <- lmout
     }
     Ni. <- tapply(ratings,users,length) # number of ratings per user
     ydots$Ni. <- Ni.
     N.j <- tapply(ratings,users,length) # number of ratings per item
     ydots$N.j <- N.j
  } 
  class(ydots) = 'ydotsMM'
  invisible(ydots)
}

######################  predict.ydotsMM()  ############################

# predict() method for the 'ydotsMM' class

# arguments

#    ydotsObj: the output of trainMM()
#    testSet: data frame in same form as ratingsIn above except that there 
#             is no ratings column; thus covariates, if any, are shifted
#             leftward one slot, i.e. userID, itemID, cov1, cov2...;
#             note that the names must be the same as in the training set

# returns vector of predicted values for testSet
predict.ydotsMM = function(ydotsObj,testSet) 
{
   # see comment on as.character() above; this gets tricky; we will move
   # back and forth between referring to elements by name and by ordinal
   # index, i.e. w['b'] vs. w[28]
   ts1 <- as.character(testSet[,1])  # user IDs, char form
   ts2 <- as.character(testSet[,2])  # item IDs, char form
   usrMeans <- ydotsObj$usrMeans[ts1]  # using named elements
   itmMeans <- ydotsObj$itmMeans[ts2]  # using named elements
   # make all terms in sums below have consistent element names!;
   # essentially, we are naming the elements of both usrMeans and
   # itmMeans after the elements of testSet[,1]
   names(itmMeans) <- ts1
   # now start building the prediction
   nTest <- nrow(testSet)
   haveCovs <- ncol(testSet) > 2
   if (!haveCovs) {
      pred <- usrMeans + itmMeans - ydotsObj$grandMean
   }
   else {
      # where are the covariates?
      usrCovCols <- ydotsObj$usrCovCols 
      itmCovCols <- ydotsObj$itmCovCols
      # shift left due to no Ratings column
      if (!is.null(usrCovCols)) usrCovCols <- usrCovCols - 1 
      if (!is.null(itmCovCols)) itmCovCols <- itmCovCols - 1 
      # must center the covariates, using the same centering information
      # used in ydotsObj
      colmeans <- ydotsObj$covMeans
      colmeans <- matrix(rep(colmeans,nTest),nrow=nTest,byrow=TRUE)
      testSet[,-(1:2)] <- testSet[,-(1:2)] - colmeans

      # start with the mu term
      Y.. <- ydotsObj$grandMean
      pred <- rep(Y..,nTest) 
      
      # now the alpha term:
      if(!is.null(usrCovCols)) {
         predalpha <- 
            predict(ydotsObj$lmoutUsr,testSet[,usrCovCols,drop=FALSE])
      } else {
         Yi. <- ydotsObj$usrMeans
         testUsrNames <- as.character(testSet[,1])
         predalpha <- Yi.[testUsrNames] - Y..
      }
      
      # and beta:
      if(!is.null(itmCovCols)) {
         predbeta <- 
            predict(ydotsObj$lmoutItm,testSet[,itmCovCols,drop=FALSE])
      } else {
         Y.j <- ydotsObj$itmMeans
         testItmNames <- as.character(testSet[,2])
         predbeta <- Y.j[testItmNames] - Y..
      }

      # and finally
      pred <- pred + predalpha + predbeta
   }
   names(pred) <- NULL  # use ordinal indexing
   pred
}

