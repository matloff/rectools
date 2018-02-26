
# Motivation:

# In the MM and MLE latent-factor models in this package, we have
 
#    Y_ij = mu + alpha_i + beta_j + eps
 
# But the above equation looks like a regression equation, so we might
# treat the estimates of alpha_i and beta_j, which are the per-user and
# per-item sample means Yi. and Y.j, as predictor variables in a
# regression context, i.e. we predict Y_ij from:

#    Yi.
#    Y.j
#    optional covariates depending on i, j or i and j jointly

# For instance, for the MovieLens data, covariates for user i might be 
# age and gender, those for item j might be genre, and a joint
# covariate might be user i's general liking for the genre of item j.

# It seems intuitive that Yi. and Y.j would be
# reasonable candidates for predictors. 

# Any regression model can be used, e.g. linear, logistic, random
# forests, neural networks, etc.

# arguments:

#    ratingsIn: the raw input data, each having the form 
#               (userID, itemID, rating, covariates)
#    regModel: the function to use for regression, such as lm() or
#              randomForest()
#    ydotsObj: output from trainMM(), to get the user and item means; if
#              NULL, will be generated here
#    rmArgs: regression model arguments, e.g. number of nearest
#            neighbors; expressed as a quoted string; not implemented yet

# IMPORTANT NOTE (repeated here from trainMM.R):
# user and item IDs may not be consecutive; even if they are
# consecutive in the original, if we do cross-validation, this 
# may not be the case; so we switch to character IDs

regressYdots <- function(ratingsIn,regModel='lm',ydotsObj=NULL,rmArgs=NULL) 
{
   covs <- as.matrix(ratingsIn[,-(1:3)])  # NULL if no covariates
   if (!is.null(covs)) {
      covs <- as.data.frame(covs)
      colnames(covs) <- names(ratingsIn[,-(1:3)]) 
   }
   if (is.null(ydotsObj)) ydotsObj <- trainMM(ratingsIn[,1:3])
   usrMeans <- ydotsObj$usrMeans
   itmMeans <- ydotsObj$itmMeans
   # as explained above, usrMeans and itmMeans are indexed by character
   # versions of user/item IDs; thus again, the next 2 lines are needed 
   # due to character indexing of usrMeans and itmMeans
   usrsInput <- as.character(ratingsIn[,1])
   itmsInput <- as.character(ratingsIn[,2])
   uMeans <- usrMeans[usrsInput]
   iMeans <- itmMeans[itmsInput]
   # uMeans, iMeans have length = nrow(ratingsIn); e.g. i-th element of
   # uMeans is Yi.
   xy <- data.frame(uMeans,iMeans,covs,ratingsIn[,3])
   x <- data.frame(uMeans,iMeans,covs)
   y <- ratingsIn[,3]
   names(xy) <- c('uMeans','iMeans',names(covs),'rats')
   if (regModel %in% c('lm','rf','ctree')) {
      cmd <- paste0(regModel,'(')
      cmd <- paste0(cmd,'rats ~ .,data=xy')
      if (!is.null(rmArgs)) {
         cmd <- paste0(cmd,'.',rmArgs,')')
      } else cmd <- paste0(cmd,')')
      return(eval(parse(text=cmd)))
   }
   if (regModel == 'rf') {
      require(randomForest)
      return(randomForest(x,y))
   } 
}

