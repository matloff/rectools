
# Motivation:

# In the MM and MLE latent-factor models in this package, we have
 
#    Y_ij = mu + alpha_i + beta_j + eps
 
# But the above equation looks like a regression equation, so we might
# treat estimates of alpha_i and beta_j in the form of per-user and
# per-item sample means Yi. and Y.j, as predictor variables in a
# regression context, i.e. we predict Y_ij from:

#    Yi.
#    Y.j
#    optional covariates depending on i, j or i and j jointly

# For instance, for the MovieLens data, covariates for user i might be 
# age and gender, those for item j might be genre, and a joint
# covariate might be user i's general liking for the genre of item j.

# It seems intuitive that Yi. and Y.j would be reasonable candidates for
# predictors, regardless of whether the above model is correct.  (Note
# that the model for the regression function amy still be correct; the
# regression function of Y on alpha and beta does exist, where mu+alpha
# and mu+beta are the limits of Yi. and Y.j

# Any regression model can be used, e.g. linear, logistic, random
# forests, neural networks, etc.

# arguments:

#    ratingsIn: the raw input data, each having the form 
#               (userID, itemID, rating, covariates)
#    regModel: the function to use for regression; current choices are
#              'lm'; 'glm' with family = binomial; 'rf' (random forest);
#              'poly' (polyreg package)
#              
#    rmArgs: regression model arguments, e.g. number of nearest
#            neighbors; expressed as a quoted string; not implemented yet

# value:

#    whatever the regression function returns

# IMPORTANT NOTE (repeated here from trainMM.R):
# user and item IDs may not be consecutive; even if they are
# consecutive in the original, if we do cross-validation, this 
# may not be the case; so we switch to character IDs

# when predicting new observations, don't forget to call IDstoMeans() on
# the new data

regressYdots <- function(ratingsIn,regModel='lm',rmArgs=NULL) 
{

   x <- IDsToMeans(ratingsIn)
   y <- ratingsIn[,3]
   xy <- cbind(x,y)
   names(xy) <- c(names(x),'rats')

   # perform the regression analysis
   if (regModel %in% c('lm','glm')) {
      cmd <- paste0(regModel,'(')
      cmd <- paste0(cmd,'rats ~ .,data=xy')
      if (!is.null(rmArgs)) {
         cmd <- paste0(cmd,',',rmArgs,')')
      } else cmd <- paste0(cmd,')')
      return(eval(parse(text=cmd)))
   }
   if (regModel == 'rf') {
      require(randomForest)
      return(randomForest(x,y))
   } 
   if (regModel == 'poly') {
      require(polyreg)
      cmd <- 'polyFit(xy,'
      cmd <- paste0(cmd,rmArgs,')')
      return(eval(parse(text=cmd)))
   } 
}

# converts the first two columns of ratingsIn from user/item IDs to
# user/item ratings means; copies the covs (columns 4+, if any); adjusts
# column names accordingly
IDsToMeans <- function(ratingsIn) 
{
   # handle covariates first
   covs <- as.matrix(ratingsIn[,-(1:3)])  # NULL if no covariates
   if (!is.null(covs)) {
      covs <- as.data.frame(covs)
      colnames(covs) <- names(ratingsIn[,-(1:3)]) 
   }

   users <- as.character(ratingsIn[,1])
   items <- as.character(ratingsIn[,2])
   ratings <- ratingsIn[,3]

   # could add code to allow reusing previous computation
   Ni. <- tapply(ratings,users,length) # number of ratings per user
   N.j <- tapply(ratings,items,length) # number of ratings per item
   usrMeans <- tapply(ratings,users,mean)
   itmMeans <- tapply(ratings,items,mean)
   
   # as explained above, usrMeans and itmMeans are indexed by character
   # versions of user/item IDs; thus again, the next 2 lines are needed 
   # due to character indexing of usrMeans and itmMeans
   usrsInput <- as.character(ratingsIn[,1])
   itmsInput <- as.character(ratingsIn[,2])
   uMeans <- usrMeans[usrsInput]
   iMeans <- itmMeans[itmsInput]
   # uMeans, iMeans have length = nrow(ratingsIn); e.g. i-th element of
   # uMeans is Yi.
   uN <- Ni.[usrsInput]
   iN <- N.j[itmsInput]
   means <- data.frame(uMeans=uMeans,iMeans=iMeans,uN=uN,iN=iN)
   names(means) <- c('uMeans','iMeans','uN','iN')
   x <- cbind(means,covs)
   x
}
