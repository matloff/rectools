\name{buildMatrix}
\alias{buildMatrix,toUserItemRatings}
\title{Build Ratings Matrix}

\description{
Convert to and from the (userID, itemID, rating) data and ratings matrix.
}

\usage{
buildMatrix(ratingsIn)
toUserItemRatings(ratMat)
}

\arguments{
   \item{ratingsIn}{Raw data, in (user ID, item ID, rating) format.}
   \item{ratMat}{Ratings matrix}
}
\details{
The \code{buildMatrix} function inputs a user ratings matrix, and
returns the ratings matrix, with element (i,j) containing the rating of
item j by user i.

The \code{toUserItemRatings} function converts in the opposite
direction.
}

\author{
	Pooja Rajkumar, Norm Matloff
}

