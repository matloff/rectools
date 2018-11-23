
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
# that the model for the regression function may still be correct; the
# regression function of Y on alpha and beta does exist, where mu+alpha
# and mu+beta are the limits of Yi. and Y.j as the sample size grows.)

# Furthermore, the numbers Ni. and N.j of ratings per user and per item
# may be informative too.

# Thus the function convertX() converts the input data ratingIns
# to a new data frame, whose row k is:

# alpha.hat_U(k)
# beta.hat)I(k)
# Ni._U(k)
# N.j_I(k)
# covars_k from ratingsIn[k,]

# where U(k) and I(k) are ratingsIn[k,1] and ratingsIn[k,2]

# Any regression model can be used, e.g. linear, logistic, random
# forests, neural networks, etc.

# arguments:

#    ratingsIn: the raw input data, each having the form 
#               (userID, itemID, rating, covariates)
#    regModel: the function to use for regression; current choices are
#              'lm'; 'glm' with family = binomial; 'dn' (deepnet
#              neural nets package); 'poly' (polyreg package)
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

trainRegYdots <- function(ratingsIn,regModel='lm',rmArgs=NULL) 
{

   uinn <- getUINN(ratingsIn)
   x <- convertX(ratingsIn,uinn)
   y <- ratingsIn[,3]
   xy <- cbind(x,y)
   names(xy) <- c(names(x),'rats')
   rownames(x) <- rownames(ratingsIn)

   # ultimately the return value
   result <- list(regModel=regModel,x=x, y=y,UINN=uinn)  
   class(result) <- 'regYdots'

   # perform the regression analysis
   if (regModel %in% c('lm','glm')) {
      cmd <- paste0(regModel,'(')
      cmd <- paste0(cmd,'rats ~ .,data=xy')
      if (!is.null(rmArgs)) {
         cmd <- paste0(cmd,',',rmArgs,')')
      } else cmd <- paste0(cmd,')')
      regOut <- eval(parse(text=cmd))
   } else if (regModel == 'dn') {
      require(deepnet)
     
      x <- as.matrix(x)
      x <- scale(x)
      result$x <- x
      cmd <- 'nn.train(x,y,'
      cmd <- paste0(cmd,rmArgs,')')
      regOut <- eval(parse(text=cmd))
   } else if (regModel == 'poly') {
      require(polyreg)
      cmd <- 'polyFit(xy,'
      cmd <- paste0(cmd,rmArgs,')')
      regOut <- eval(parse(text=cmd))
   } 

   result$regOut <- regOut
   result
}

# for each user, find the mean rating and number of ratings, and
# similarly for each item; users and items indexed by the character
# forms of their IDs

getUINN <- function(ratingsIn) 
{
   users <- as.character(ratingsIn[,1])
   items <- as.character(ratingsIn[,2])
   ratings <- ratingsIn[,3]

   Ni. <- tapply(ratings,users,length) # number of ratings per user
   N.j <- tapply(ratings,items,length) # number of ratings per item
   usrMeans <- tapply(ratings,users,mean)
   itmMeans <- tapply(ratings,items,mean)
   list(uMeans=usrMeans,iMeans=itmMeans,uN=Ni.,iN=N.j)
}

# converts (user,item,covs) data to (usermean,itemmean,Ni.,N.j,covs)

convertX <- function(ratingsIn,UINN) 
{
   # handle covariates first
   if (ncol(ratingsIn) > 3) {
      covs <- as.matrix(ratingsIn[,-(1:3)])  # NULL if no covariates
      dimnames(covs)[[2]] <- names(ratingsIn[,-(1:3)]) 
   } 
   
   # as explained above, usrMeans and itmMeans are indexed by character
   # versions of user/item IDs; thus again, the next 2 lines are needed 
   # due to character indexing of usrMeans and itmMeans
   usrsInput <- as.character(ratingsIn[,1])
   itmsInput <- as.character(ratingsIn[,2])
   # now need to convert each input row to (usermean,itemmean,covs)
   userMeans <- UINN$uMeans[usrsInput]
   itemMeans <- UINN$iMeans[itmsInput]
   userN <- UINN$uN[usrsInput]
   itemN <- UINN$iN[itmsInput]

   # start forming the converted data
   means <- 
      data.frame(uMeans=userMeans,iMeans=itemMeans,userN=userN,itemN=itemN)
   names(means) <- c('uMeans','iMeans','uN','iN')
   if (ncol(ratingsIn) > 3) {
      x <- cbind(means,covs)
   } else x <- means
   rownames(x) <- rownames(ratingsIn)
   x

}

predict.regYdots <- function(rgydObj,newdata) 
{
   if (rgydObj$regModel != 'dn') {
      predict(rgydObj$regOut,newdata)
   } else {
      nn.predict(rgydObj$regOut,newdata)
   }
}
