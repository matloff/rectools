
# motivation:

# the MM and MLE models have
 
#    Y_ij = mu + alpha_i + beta_j + eps
 
# we then estimate the mu, alpha_i and beta_j using MM or MLE; in the MM
# case, the estimated alpha_i is the mean of all known ratings from user
# i, minus mu, which is estimated by the overall mean of all ratings
 
# but the above equation looks like a regression equation, so we might
# treat the estimates of alpha_i and beta_j, which are the per-user and
# per-item sample means Yi. and Y.j, as predictor variables in a
# regression context; it seems intuitive that Yi. and Y.j would be
# reasonable candidtes for predictors, and of course one can add
# along with any covariates, whether user-specific, item-specific or
# pertaining to both user and item 

# arguments:

#    ratingsIn: the raw input data, each having the form 
#               (userID, itemID, rating, covariates)
#    regModel: the function to use for regression, such as lm() or
#              randomForest()
#    ydotsObj: output from trainMM(), to get the user and item means
#    rmArgs: regression model arguments, e.g. number of nearest
#            neighbors; expressed as a quoted string; not implemented yet

regressYdots <- function(ratingsIn,regModel='lm',ydotsObj=NULL,rmArgs=NULL) 
{
   if (is.null(ydotsObj)) ydotsObj <- trainMM(ratingsIn)
   usrMeans <- ydotsObj$usrMeans
   itmMeans <- ydotsObj$itmMeans
   covs <- as.matrix(ratingsIn[,-(1:3)])  # NULL if no covariates
   # next 2 lines needed due to indexing of usrMeans and itmMeans; see
   # comment in trainReco()
   usrsChar <- as.character(ratingsIn[,1])
   itmsChar <- as.character(ratingsIn[,2])
   uMeans <- usrMeans[usrsChar]
   iMeans <- itmMeans[itmsChar]
   # uMeans, iMeans have length same as nrow(ratingsIn)
   xy <- data.frame(uMeans,iMeans,covs,ratingsIn[,3])
   x <- data.frame(uMeans,iMeans,covs)
   y <- ratingsIn[,3]
   names(xy) <- c('uMeans','iMeans',names(covs),'rats')
   if (regModel == 'lm') return(lm(rats ~ .,data=xy)) 
   if (regModel == 'rf') {
      require(randomForest)
      return(randomForest(x,y))
   } 
   if (regModel == 'ctree') {
      require(partykit)
      return(ctree(rats ~ .,data=xy))
   }
}

