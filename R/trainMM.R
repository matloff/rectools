
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
   # see comment on as.character() above; this gets tricky; we will move
   # back and forth between referring to elements by name and by ordinal
   # index, i.e. w['b'] vs. w[28]
   ts1 <- as.character(testSet[,1])  # user IDs, char form
   ts2 <- as.character(testSet[,2])  # item IDs, char form
   usrMeans <- ydotsObj$usrMeans[ts1]  # with named elements
   itmMeans <- ydotsObj$itmMeans[ts2]  # with named elements
   # make all terms in sums below have consistent element names!
   names(itmMeans) <- ts1
   pred <- vector(length=nrow(testSet))  # will be our return value
   names(pred) <- NULL  # pred will always use ordinal indexing
   haveCovs <- ncol(testSet) > 2
   if (!haveCovs) {
      pred <- usrMeans + itmMeans - ydotsObj$grandMean
   }
   else {
      # must center the covariates, using the same centering information
      # used in ydotsObj
      colmeans <- ydotsObj$covmeans
      testSet[,-(1:2)] <- 
         scale(testSet[,-(1:2)],center=colmeans,scale=FALSE)
      # which ones to use regression on
      # first, ordinal indices, then named indices
      smallNiUsersWhich <- which(ydotsObj$Ni[ts1] < minN)
      smallNiUsers <- ts1[smallNiUsersWhich]
      bigNiUsersWhich <- which(ydotsObj$Ni[ts1] >= minN)
      bigNiUsers <- ts1[bigNiUsersWhich]
      smallNiItems <- ts2[ydotsObj$Ni[ts1] < minN]
      bigNiItems <- ts2[ydotsObj$Ni[ts1] >= minN]
      # could use ifelse() here, but gets quite messy; better to fill in
      # the 'pred' vector's 2 portions in 2 separate actions
      # non-regression cases:
      pred[bigNiUsersWhich] <- 
         ydotsObj$usrMeans[bigNiUsers] + ydotsObj$itmMeans[bigNiItems] -
            ydotsObj$grandMean
      # regression cases:
      pred[smallNiUsersWhich] <- 
         if (haveBoth) {  # have both user and item covs
            ydotsObj$grandMean + predict(ydotsObj$lmout,
               testSet[smallNiUsersWhich,-(1:2),drop=FALSE])
         } else if (haveUserCovs) {  # have user covs only
            # note that + and - Y.. terms cancel
            predict(ydotsObj$lmout,
               testSet[smallNiUsersWhich,-(1:2),drop=FALSE]) +
               ydotsObj$itmMeans[smallNiItems]
         } else  {  # have item covs only
            # note that + and - Y.. terms cancel
            predict(ydotsObj$lmout,
               testSet[smallNiUsersWhich,-(1:2),drop=FALSE]) +
               ydotsObj$usrMeans[smallNiUsers]
         }
   }  # end covs section
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

