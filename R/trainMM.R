
# Method of Moments approach

# TODO:

#    accommodate matrices in addition to data frames, in order to all
#    use of bigmemory package

#    allow covariates that jointly involve users and items, e.g. general
#    moviegoer genre preferences

#  modified version of earlier trainMM() etc., May 27, 2017, with
#  new approach to use of covariates X_ijk for user i, item j

#  NOTE: the covariates will be centered; and since we will be
#  regressing a random effect alpha or beta on the covariates, then the
#  regression should be without an intercept term (use of -1 when
#  specifying predictors in lm())

#  NOTE: regression function choices currently limited to lm(), glm(),
#  as the -1 option is used in specifying that there is no intercept
#  term

#  basic model is
#  
#     Y = mu + alpha + beta + eps 
#    
#  for simplicity, the following discussion is at the population level

#  after regressing alpha and/or beta on the vector X of covariates then
#  either alpha or beta or both are replaced by X in the prediction;
#  e.g. if X depends only on the user covariates V, then our prediction
#  is
#
#     mu + gamma'V + beta
#
#  where gamma is the vector of regression coefficients; if we have both
#  user covariates V and item covariates W, the prediction is
#
#     mu + gamma'V + eta'W

#  at the sample level, mu + gamma'V + beta becomes
#
#     hat(Y_ij) = hat(mu) + hat(gamma)'V_i + hat(beta_j)
#
#  set Y.. to the mean of all Y_ij, Y.j the mean of all Y_i,j fixed j
#  etc.; the hat(mu) is Y.., hat(beta_j) is Y.j - Y.. etc.

# the coefficients can be estimated via lm() without a const term

#########################  trainMM()  ##############################

# arguments:

#   ratingsIn: input data, with cols (userID,itemID,rating,
#              covariates); data frame; user covariates, if any, must
#              precede item covariates, if any
#   userCovsStartCol: start column of user covariates, if any
#   itemCovsStartCol: start column of user covariates, if any

# value:

#   S3 class of type "ydots", with components:

#      Y..: grand mean, est of mu 
#      Yi.: vector of mean ratings for each user, ests. of alpha_i
#      Y.j: vector of mean ratings for each item, ests. of betaa_j
#      usrCovCols: column numbers of the user covariates, if any
#      itmCovCols: column numbers of the item covariates, if any
#      lmoutUsr: object returned by running regression analysis for user
#                covariates, if any
#      lmoutItm: object returned by running regression analysis for item
#                covariates, if any

trainMM <- function(ratingsIn,userCovsStartCol,itemCovsStartCol)
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
  ydots$trainingUsers <- unique(users)
  ydots$trainingItems <- unique(items)
  covCols <- getCovCols(userCovsStartCol,itemCovsStartCol,ncol(ratingsIn))
  usrCovCols <- covCols[1]
  itmCovCols <- covCols[2]
  ydots$usrCovCols <- usrCovCols
  ydots$itmCovCols <- itmCovCols
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
     # but treating the latter is random too, not needed;
     # user covs first, if any
     if (!is.null(usrCovCols)) {
        frml <- as.formula(paste(nms[3],'~ .-1'))
        lmout <- lm(frml,data=ratingsIn[,c(3,usrCovCols)])
        ydots$lmoutUsr <- lmout
     }
     # now item covs, if any
     if (!is.null(usrCovCols)) {
        frml <- as.formula(paste(nms[3],'~ .-1'))
        lmout <- lm(frml,data=ratingsIn[,c(3,itmCovCols)])
        ydots$lmoutItm <- lmout
     }
     Ni <- tapply(ratings,users,length) # number of ratings per user
     ydots$Ni <- Ni
  } 
  class(ydots) = 'ydotsMM'
  invisible(ydots)
}

######################  predict.ydotsMM()  ############################

# predict() method for the 'ydotsMM' class

# in predicting for user i, the code looks at N_i, the number of ratings
# by user i; if that number is below minN, the prediction comes from
# user i's covariate information (if available) instead of from the
# Y.j values

# arguments

#    ydotsObj: the output of trainMM()
#    testSet: data frame in same form as ratingsIn above except that there 
#             is no ratings column; thus covariates, if any, are shifted
#             leftward one slot, i.e. userID, itemID, cov1, cov2...;
#             note that the names must be the same as in the training set
#    minN:  if Ni < minN and have covariates, use the latter instead of
#           Yi.; see above

# returns vector of predicted values for testSet
predict.ydotsMM = function(ydotsObj,testSet,minN=0) 
{
   # see comment on as.character() above; this gets tricky; we will move
   # back and forth between referring to elements by name and by ordinal
   # index, i.e. w['b'] vs. w[28]
   ts1 <- as.character(testSet[,1])  # user IDs, char form
   ts2 <- as.character(testSet[,2])  # item IDs, char form
   usrMeans <- ydotsObj$usrMeans[ts1]  # with named elements
   itmMeans <- ydotsObj$itmMeans[ts2]  # with named elements
   # make all terms in sums below have consistent element names!
   names(itmMeans) <- ts1
   nTest <- nrow(testSet)
   haveCovs <- ncol(testSet) > 2
   if (!haveCovs) {
      pred <- usrMeans + itmMeans - ydotsObj$grandMean
   }
   else {
      usrCovCols <- ydotsObj@usrCovCols
      itmCovCols <- ydotsObj$itmCovCols
      if (minN == 0) stop('with covariates, need minN > 0')
      # must center the covariates, using the same centering information
      # used in ydotsObj
      colmeans <- ydotsObj$covmeans
      testSet[,-(1:2)] <- testSet[,-(1:2)] - colmeans
      # which cases to use covariates on, i.e. which users or cases have
      # only a small number of data points?
      smallNiUsersWhich <- which(ydotsObj$Ni[ts1] < minN)
      smallNiUsers <- ts1[smallNiUsersWhich]
      bigNiUsersWhich <- which(ydotsObj$Ni[ts1] >= minN)
      bigNiUsers <- ts1[bigNiUsersWhich]
      smallNiItems <- ts2[ydotsObj$Ni[ts1] < minN]
      bigNiItems <- ts2[ydotsObj$Ni[ts1] >= minN]
      # they all start with the mu term
      Y.. <- ydotsObj$grandMean
      pred <- rep(Y..,nTest) 
      # now the alpha and beta terms; first find the regression-based
      # predictions of all alpha, beta, even though only use some
      predalpha <- predict(ydotsObj$lmoutUsr,testSet[,usrCovCols])
      predbeta <- predict(ydotsObj$lmoutItm,testSet[,usrCovCols])
      # now add those to the cases of small numbers of users or items
      pred[smallNiUsers] <- pred[smallNiUsers] + predalpha[smallNiUsers]
      pred[smallNiItems] <- pred[smallNiItems] + predalpha[smallNiItems]
      # now add the non-regression predictions to the cases of big numbers 
      # of users or items
      pred[bigNiUsers] <- pred[bigNiUsers] + usrMeans[bigNiUsers] - Y.. 
      pred[bigNiItems] <- pred[bigNiItems] + itmMeans[bigNiItems] - Y..
      # pred[bigNiUsersWhich] <- 
      #    ydotsObj$usrMeans[bigNiUsers] + ydotsObj$itmMeans[bigNiItems] -
      #       ydotsObj$grandMean
   }
   names(pred) <- NULL  # use ordinal indexing
   pred
}

# test case
checkMM <- function() {
   set.seed(99999)
   d <- data.frame(u=sample(11:15,12,replace=T),i=sample(26:30,12,replace=T),
           r=sample(1:5,12,replace=T),cv1=runif(12))
   ts <- data.frame(u=sample(11:15,5,replace=T),i=sample(26:30,5,replace=T),
           cv1=runif(5))
   yd <- trainMM(d)
   predict(yd,ts)
   # 5.4166667 0.6666667 0.6666667 2.6666667 3.6666667
   predict(yd,ts,haveItemCovs=T,minN=2)
   # 5.4166667 -0.3306984  0.5349273  3.0111962  3.6666667
   predict(yd,ts,haveUserCovs=T,minN=2)
   # 5.416667 1.669302 2.534927 3.011196 3.666667
}

