\name{cosDist,formUserDat}
\alias{cosDist}
\alias{formUserData}

\title{Cosine-Based Analysis}

\description{
Similarity-based analysis via inner products of user ratings, and
possibly other variables. 
}


\usage{
formUserData(ratingsIn, usrCovs = NULL, itmCats = NULL, fileOut = "") 
cosDist(x, y, wtcovs, wtcats)
predict.usrData(origData, newData, newItem, k, wtcovs = NULL, wtcats = NULL)
}

\arguments{
	\item{ratingsIn}{Input data frame, training set. Within-row format 
           is (UserID, ItemID, rating).}
        \item{usrCovs}{Data frame of user covariates, e.g. gender and
           age, one row per user.  User i must be in row i.}
        \item{itmCats}{Data frame of item categories, e.g. movie genre,
           one row per item}
        \item{x}{Object of class \code{usrDatum}.}
        \item{y}{Object of class \code{usrDatum}.}
        \item{wtcovs}{Weight to be placed on covariates, relative to
           ratings variables.  Must be positive if have covariates.}
        \item{wtcats}{Weight to be placed on categories, relative to
           ratings variables.}
        \item{origData}{Object of class \code{'usrData'}, serving as the
           training set.}
        \item{newData}{Object of class \code{'usrDatum'}, to be
           predicted.}  
	\item{newItem}{The item ID of the rating to be predicted.}
	\item{k}{Number of nearest neighbors.}
        \item{fileOut}{A file name.}
}

\details{

The function \code{formUserData} inputs the usual (user ID,item ID,rating)
data, and outputs an R list, of class \code{usrData} that has one 
element per user ID.  That element, of class \code{usrDatum}, has the
following components:

\itemize{

   \item \code{userID:} User ID.
   \item \code{itms:} Vector of IDs for items rated by this user.
   \item \code{ratings:} Vector of ratings for those items.
   \item \code{usrCovs:} Vector of values of covariates, e.g. gender and
      age, for this user.
   \item \code{itmCats:} Vector of proportions for the item categories
      (need not sum to 1) for this user.  The j-th one is the proportion
      of items rating by this user in item category j.
}

There is no training code; to perform prediction, the only preparation
is calling \code{formUserData}, which produces a kind of "training set"
of class \code{usrData} for input into the \code{predict} method
\code{predict.usrData}.  The latter predicts (at present) a single new
case at a time, based on the data "nearest" the new case, as follows.

In \code{cosDist}, the "distance" (not actually a mathematical metric)
between numeric vectors u and v is defined to be (u,v) / sqrt((u,u)
(v,v)), where ( , ) means inner product.  The function \code{cosDist}
finds this for two objects of class \code{'usrDatum'}, with the inner
product being taken on the ratings contained in each of these objects,
as well as the covariates and category data if any. 

}

\examples{
ivl <- InstEval 
ivl$s <- as.numeric(ivl$s) 
ivl$d <- as.numeric(ivl$d) 
ivl <- ivl[,c(1,2,7)] 
ivl10 <- ivl[1:10,] 
ivl10ud <- formUserData(ivl10)
ivl10ud[[1]]
# output of last is
# $userID  
# [1] 1  
#   
# $itms  
# [1]  525  560  832 1068  
#   
# $ratings  
# [1] 5 2 5 3  
#   
# attr(,"class")  
# [1] "usrDatum"  
}

\author{
Norm Matloff and Vishal Chakraborty
}
