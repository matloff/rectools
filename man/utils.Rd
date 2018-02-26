\name{ratingness,covratingness,getML100K,getInstEval}
\alias{ratingness}
\alias{covratingness}
\alias{getML100K}
\alias{getInstEval}

\title{Utilities}

\description{Miscellaneous utility functions.}

\usage{
ratingness(ratingsIn,splitCol)
covratingness(ratingsIn,splitCol)
getInstEval()
}

\arguments{
  \item{ratingsIn}{Input data frame.  Within-row format is UserID,
     ItemID, Rating and optional covariates.}
  \item{splitCol}{Column to count ratings for -- 1 for user ratings, 2
     for item ratings.}
}

\details{

   The InstEval and 100K MovieLens datasets serve as examples and
   testbeds for many methods in this package.  Since each requires some
   amount of setup, we encapsulate the setup operations in the
   \code{getInstEval} and \code{getML100K} functions.

   The fact that some users rate more items may itself be useful  as a
   predictor.  Frequent rates, for instance, may be more liberal in
   their ratings, so this variable may be important to include as a
   covariate.

   A typical usage is to call \code{covratingness} and then append the
   result to the input data frame.

}

\examples{
rtin <- data.frame(uid = c(1,3,2,1,2),itemid = c(1,2,1,2,2), rats=c(2,5,3,5,1))
ratingness(rtin,1)  
# returns (2,2,1), meaning that user 1 submitted 2 ratings, etc.
> covratingness(rtin,1)
# returns a 5-element vector, corresponding to the 5 rows of rtin
rtin$nrats <- covratingness(rtin,1)
# now usable as a predictor
}

\author{
Norm Matloff 
}

