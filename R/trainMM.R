
# Method of Moments approach

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

#      alphai: vector of ests. of alpha_i
#      betaj: vector of ests. of beta_j
#      lmout: object returned by running the regression analysis 
#      Ni.: vector of number of ratings by each user
#      N.j: vector of number of ratings of each item

trainMM <- function(ratingsIn)
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
  haveCovs <- ncol(ratingsIn) > 3

  n <- nrow(ratingsIn)
  userRowGrps <- split(1:n,users)
  itemRowGrps <- split(1:n,items)

  uimean <- function(uirowgrp) mean(ratings[uirowgrp])
  Yi. <- sapply(userRowGrps,uimean)
  Y.j <- sapply(itemRowGrps,uimean)


  if (haveCovs) {
     # get gammahat and associated 'lm' object
     cmd <- paste('lmout <- lm(', nms[3],
               ' ~ .,data=ratingsIn[,-(1:2)])', sep='')
     eval(parse(text=cmd))
     xmeans <- 
        function(uirowgrp) colMeans(ratingsIn[uirowgrp,-(1:3),drop=FALSE])
     nCovs <- ncol(ratingsIn) - 3
     Xi. <- sapply(userRowGrps,xmeans)
     if (nCovs == 1)  {
        Xi. <- matrix(Xi.,nrow=1)
        row.names(Xi.) <- names(ratingsIn)[4]
     }
     Xi. <- t(Xi.) 
     X.j <- sapply(itemRowGrps,xmeans)
     if (nCovs == 1)  {
        X.j <- matrix(X.j,nrow=1)
        row.names(X.j) <- names(ratingsIn)[4]
     }
     X.j <- t(X.j)
     predsa <- predict(lmout,as.data.frame(Xi.))
     predsb <- predict(lmout,as.data.frame(X.j))
  } else {
     Y.. <- mean(ratings)
     predsa <- Y..
     predsb <- Y..
  }
  
  alphai <- Yi. - predsa
  betaj <- Y.j - predsb

  ydots <- list(alphai=alphai,betaj=betaj)
  if (haveCovs) ydots$lmout <- lmout else ydots$Y.. <- Y..
  Ni. <- tapply(ratings,users,length) # number of ratings per user
  ydots$Ni. <- Ni.
     N.j <- tapply(ratings,users,length) # number of ratings per item
     ydots$N.j <- N.j
  class(ydots) = 'ydotsMM'
  invisible(ydots)
}

######################  trainMMpar()  ############################

# probably useful only if ratingsIn is already distributed, either read
# it from a distributed file or the result of a previous call to
# distribsplit(); signified by the argument ratingsIn being a character
# string specifying the name of the distributed object

trainMMpar <- function(ratingsIn,cls)
{
   require(partools)
   require(rectools)
   if (!is.character(ratingsIn)) {
      distribsplit(cls,'ratingsIn')
      warning('parallel version likely slow here')
   } else {
      rIn <- ratingsIn
      clusterExport(cls,'rIn',envir=environment())
      clusterEvalQ(cls,cmd <- paste('ratingsIn <<- ',rIn))
      clusterEvalQ(cls,eval(parse(text=cmd)))
   }
   tmp <- clusterEvalQ(cls,mmout <- trainMM(ratingsIn))
   mmout <- list()
   class(mmout) <- 'ydotsMMpar'
   mmout
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
   usrAlphas <- ydotsObj$alphai[ts1]  # using named elements
   itmBetas <- ydotsObj$betaj[ts2]  # using named elements
   # make all terms in sums below have consistent element names!;
   # essentially, we are naming the elements of both usrMeans and
   # itmMeans after the elements of testSet[,1]
   names(itmBetas) <- ts1
   nTest <- nrow(testSet)
   haveCovs <- ncol(testSet) > 2
   # build the predictions vector
   preds <- vector(length=nTest)
   if (!haveCovs) {
      preds[] <- ydotsObj$Y..
   } else {
      preds[] <- predict(ydotsObj$lmout,testSet) 
   }
   preds <- preds + usrAlphas + itmBetas
   names(preds) <- NULL  # use ordinal indexing
   preds
}

######################  predict.ydotsMMpar()  ############################

predict.ydotsMMpar = function(ydotsObj,testSet,cls) 
{
   clusterExport(cls,'testSet',envir=environment())
   preds <- clusterEvalQ(cls,predict(mmout,testSet))
   Reduce('+',preds)/length(cls)
}

