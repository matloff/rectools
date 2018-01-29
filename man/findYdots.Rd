\name{trainMM,trainMLE,predict.ydotsMM,predict.ydotsMLE,plot.ydotsMM}
\alias{trainMM}
\alias{trainMLE}
\alias{trainMM}
\alias{trainMLE}
\alias{predict.ydotsMM}
\alias{plot.ydotsMM}
\alias{predict.ydotsMLE}

\title{Covariate-Based, Latent-Factor Recommender Systems}

\description{
Tools to incorporate user and item information into latent-factor 
recommender system methodology, and to add parallel computation
capability.  Various plots can be displayed.
}

\usage{
trainMM(ratingsIn,userCovsStartCol=NULL,itemCovsStartCol=NULL) 
predict.ydotsMM(ydotsObj,testSet,minN) 
plot.ydotsMM(ydotsObj,ratingsIn) 
trainMLE(ratingsIn,cls=NULL) 
predict.ydotsMLE(ydotsObj,testSet) 
}

\arguments{
  \item{ratingsIn}{Input data frame.  Within-row format is UserID,
     ItemID, Rating and optional covariates.}
  \item{userCovsStartCol}{Column number in \code{ratingsIn} at which the
     user-specific covariates begin.}
  \item{itemCovsStartCol}{Column number in \code{ratingsIn} at which the
     item-specific covariates begin.}
  \item{cls}{An R \code{parallel} cluster.}
  \item{minN}{If a prediction is to be made on a user with fewer than
     this number of ratings and if there are covariates, predict from
     the covariates.}
  \item{ydotsObj}{An object of class \code{'ydotsMM'} or \code{'ydotsMLE'}.}
  \item{testSet}{A data frame consisting of cases to be predicted.
     Format is the same as \code{ratingsIn}, except that there is no
     ratings column.}
}

\details{

   Note:  This software assumes that user and item ID number are
   consecutive, starting with 1.

   All functions here implement latent-factor models for recommender
   systems.  They add the capability of using covariates, e.g. age and
   gender, and in some cases enable parallel computation.  MLE and
   Method of Moments approaches are offered.

   The basic model without covariates, is 

   mean rating = overall mean + user effect + item effect

   Adding covariates, this becomes

   mean rating = linear covariates effect + user effect + item effect

   This is the model we use in the MLE case. But for the Method of
   Moments, we essentially have an identifiability problem:  The
   estimated effect for user i would be the sample mean of all his/her
   ratings, minus the overall sample mean of all ratings, minus the
   linear covariates effect.  The latter would then get added right back
   in to the sum, resulting in no impact of the covariates.  (It is not
   a direct issue with MLE, due to the additional assumption that the
   user effects are normally distributed.)  So, in the MM case, our
   model is

   mean rating = linear covariates effect + item effect

   So, by offering both MLE and MM approaches, the package not only
   allows two different estimation methods, but also two different
   models.  The MLE structural model is more general than that of MM,
   but has more restrictive distributional assumptions.

   The covariates are assumed to begin in column 4 of \code{ratingsIn},
   with the user-related ones, if any, coming first, and then the
   item-related ones, if any.

   The functions \code{trainMM} and \code{trainMLE} work on a
   training set, returning objects that later can be used to predict new
   cases.  The former is much faster than the latter and has a smaller
   memory footprint, though both shortcomings of MLE are ameliorated to
   some extent via parallel computation.

   The \code{trainMLE} function is primarily a wrapper that sets up
   Maximum Likelihood Estimation (assuming normal user and item effects)
   for a crossed-effects model in the \code{lme4} function \code{lmer}.
   As the computation for that function can be lengthy and
   memory-intensive, \code{trainMLE} also enables 
   parallelizing the computation.

   Plotting:  Calling \code{plot(ydotsObj,ratingsIn)} invokes
   \code{plot.ydotsMM}.  Several plots are displayed, including density
   estimates for the user and item random effects, and a smoothed
   scatter plot for the joint density of those effects.

}

\value{

   The functions \code{trainMM} and \code{trainMLE} return
   objects of class \code{} \code{'ydotsMM'} and \code{'ydotsMLE'},
   respectively.
   
   The functions \code{predict.ydotsMM} and \code{predict.ydotsMLE}
   return a vector of predicted ratings.
}

\examples{
# toy example
rts <- rbind(c(1,3,1),c(4,2,2),c(4,3,1),c(1,2,4))
rts <- data.frame(rts)
ydots <- trainMM(rts)
ydots  # e.g. usrMeans['4'] = 1.5
predict(ydots,rbind(c(1,2),c(4,3),c(4,2)))  # 3.5, 0.5, 2.5 
# instructor evaluation data
getInstEval()
# run the training data, no covariates
ydout <- trainMLE(ivl[,1:3]) 
# form a test set to illustrate prediction
testSet <- ivl[c(3,8),]
# say want to predict how well students 1 and 3 would like instructor 12
testSet[1,2] <- 12
testSet[2,2] <- 12
# predict
predict(ydout,testSet[,1:2])  # 4.272660 4.410612
# MM without covariates
ydout <- trainMM(ivl[,1:3])
predict(ydout,testSet[,-3])  # 5.141009 5.137111 
# try using the covariates
ydout <- trainMM(ivl,userCovsStartCol=4,itemCovsStartCol=5)
predict(ydout,testSet[,-3],minN=5)  # 5.141009 5.137111 
}

\author{
Norm Matloff and Pooja Rajkumar
}

