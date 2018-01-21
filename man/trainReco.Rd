\name{trainReco,predict.RecoS3,trainRecoPar,predict.RecoS3par}
\alias{trainReco}
\alias{predict.RecoS3}
\alias{trainRecoPar}
\alias{predict.RecoS3par}

\title{Recosystem Wrappers}

\description{
Convenience wrappers for specific aspects of the \code{recosystem}
package, including adding a parallel computation capability.
}

\usage{
	trainReco(ratingsIn,rnk=10,nmf=FALSE)
	predict.RecoS3(recoObj,testSet)
	trainRecoPar(ratingsIn,rnk=10,nmf=FALSE,cls,pqName='PQ')
	predict.RecoS3par(RecoS3parObj,testSet,cls)
}

\arguments{
	\item{ratingsIn}{Input data frame, training set. Within-row format 
           is (UserID, ItemID, rating).}
	\item{rnk}{Desired rank for the matrix factors.}
	\item{nmf}{If true, use NMF, otherwise SVD.}
        \item{recoObj}{Object of class \code{'RecoS3'}, output of
           \code{trainReco}.}
        \item{RecoS3parObj}{Object of class \code{'RecoS3par'}, output of
           \code{trainRecoPar}.}
        \item{testSet}{Data to be predicted, having within-row format
           (UserID, ItemID).}
}

\details{
The training functions call \code{r$train} on the input data.
The latter function returns several key components, including:

   \itemize{
   
   \item P: This is the user score matrix, in which a row 
   represents a user, and each column is a latent factor. 
   
   \item Q: This is the item score matrix, in which a row represents 
   an item, and each column is a latent factor.  
   }

The product of these two matricies consists of the predicted ratings of
all users on all items.

The functions \code{predict.RecoS3} and are methods for the generic
function \code{predict}.
	
}

\examples{
   getInstEval()
   ivl3 <- ivl[,1:3]
   set.seed(9999)
   trn <- trainReco(ivl3)
   onerec <- ivl3[1,]  # form dummy 1-rec data frame
   # how would student 788 would like lecturer 28?
   onerec$s <- 788  
   onerec$d <- 28  
   onerec <- onerec[,-3]
   predict(trn,onerec)  # 1.95
}

\author{
	Pooja Rajkumar, Norm Matloff and Robin Yancey
}
