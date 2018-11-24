
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

# Roles of the functions:
 
#    getUINN(ratingsIn): 
#       computes and returns the Yi., Y.j, Ni., N.j vectors
 
#    RStoReg(ratingsIn,UINN,useNij=FALSE): forms the new data frame,
#       with ratingsIn[,1:2] replaced by the Yi. and Y.j, optionally 
#       adding the Ni. and N.j as covariates; the original covariates,
#       if any, are retained; ratingsIn[,3] is moved to the last
#       column

# Thus the function RStoReg() converts the input data ratingIns
# to a new data frame, whose row k is:

# alpha.hat_U(k)
# beta.hat)I(k)
# Ni._U(k)
# N.j_I(k)
# covars_k from ratingsIn[k,]

# where U(k) and I(k) are ratingsIn[k,1] and ratingsIn[k,2]


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

RStoReg <- function(ratingsIn,useNij=FALSE) 
{
   # handle covariates first
   UINN <- getUINN(ratingsIn)
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

   # start forming the converted data
   means <- 
      data.frame(uMeans=userMeans,iMeans=itemMeans)
   # names(means) <- c('uMeans','iMeans')
   if (useNij) {
      userN <- UINN$uN[usrsInput]
      itemN <- UINN$iN[itmsInput]
      means <- cbind(means,userN,itemN)
      names(means)[3:4] <- c('uN','iN')
   }
   if (ncol(ratingsIn) > 3) {
      xy <- cbind(means,covs)
   } else xy <- means
   xy <- cbind(xy,ratingsIn[,3])
   names(xy)[ncol(xy)] <- names(ratingsIn)[3]
   rownames(xy) <- rownames(ratingsIn)
   xy
}

