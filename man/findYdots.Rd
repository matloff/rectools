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
trainMM(ratingsIn,regressYdots=FALSE,minN) 
predict.ydotsMM(ydotsObj,testSet) 
plot.ydotsMM(ydotsObj,ratingsIn) 
trainMLE(ratingsIn,cls=NULL) 
trainMLE(ratingsIn,cls=NULL) 
predict.ydotsMLE(ydotsObj,testSet) 
}

\arguments{
  \item{ratingsIn}{Input data frame.  Within-row format is UserID,
     ItemID, Rating and optional covariates.}
  \item{regressYdots}{If TRUE, apply linear regression to the latent
     factors.}
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
   systems.  They add the capability of using covariates, and in some
   cases enable parallel computation.

   The basic model is 

   mean rating = overall mean + user effect + item effect

   Adding covariates, this becomes

   mean rating = linear covariates effect + user effect + item effect

   The functions \code{trainMM} and \code{trainMLE} work on a
   training set, returning objects that later can be used to predict new
   cases.

   The \code{trainMLE} function is primarily a wrapper that sets up
   Maximum Likelihood Estimation (assuming normal user and item effects)
   for a crossed-effects model in the \code{lme4} function \code{lmer}.
   As the computation for that function can be lengthy,
   \code{trainMLE} also enables parallelizing the computation.

   The \code{trainMM} function uses the Methods of Moments instead
   of MLE.  It is much faster, and thus at present does not have a
   parallel computation capability.

   In order to accommodate possibility that the user latent factor is a
   stronger predictor than the one for items, or vice versa, the option
   \code{regressYdots = TRUE} for \code{trainMM} regresses ratings
   against user and item latent factors, enabling later prediction using
   the resulting coefficients.  This is not needed for
   \code{trainMLE}, since \code{lmer} calculates the Best Linear
   Unbiased Predictors, thus indirectly assigning weights to the user
   and item effects.

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

