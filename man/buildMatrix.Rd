\name{toUserItemRatings,toRatingsMatrix}
\alias{toUserItemRatings}
\alias{toRatingsMatrix}
\title{Move to and from Ratings Matrix}

\description{
Convert to and from the (userID, itemID, rating) data and ratings matrix.
}

\usage{
buildMatrix(ratingsIn)
toRatingsMatrix(ratingsIn, NAval = NA)
toUserItemRatings(ratMat)
}

\arguments{
   \item{ratingsIn}{Raw data, in (user ID, item ID, rating) format.  IDs
      must be R factors.}
   \item{ratMat}{Ratings matrix}
   \item{NAval}{value to place in the output matrix for missings.}
}

\details{
The \code{toRatingsMatrix} function inputs a user ratings matrix, and
returns the ratings matrix, with element (i,j) containing the rating of
item j by user i.  The \code{toUserItemRatings} function goes the
opposite way, converting a ratings matrix to an input data frame.
}

\examples{

# for InstEval data, convert to ratings matrix and back, using row 888
# as a check
ivl <- getInstEval()
ivl <- ivl[,1:3]
ivl[888,]  # (47,548,3)
ivla <- toRatingsMatrix(ivl)
ivla['47','548']  # 3
ivls <- toUserItemRatings(ivla)
w <- which(ivls$userID == '47' & ivls$itemID == '548')  #  59321
ivls[59321,]  # (47,548,3)

# HouseVoting data is already in ratings matrix form; convert to input
# data form, using case 888 as a check
hv <- getHouseVoting()
hv <- hv[,-17]  
hv[8,]  # voted no, yes, no, yes on Bills 1-4, didn't vote on Bill 15
hvs <- toUserItemRatings(hv)
w <- which(hvs$userID == '8' & hvs$itemID == 'Bill1')
hvs[w,]  # (8, Bill1, 0)
w <- which(hvs$userID == '8' & hvs$itemID == 'Bill2')
hvs[w,]  # (8, Bill2, 1)
w <- which(hvs$userID == '8' & hvs$itemID == 'Bill15')
w  # empty, didn't vote

}

\author{
	Norm Matloff
}

