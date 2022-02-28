\name{buildMatrix,toUserItemRatings,toRatingsMatrix}
\alias{buildMatrix}
\alias{toUserItemRatings}
\alias{toRatingsMatrix}
\title{Build Ratings Matrix}

\description{
Convert to and from the (userID, itemID, rating) data and ratings matrix.
}

\usage{
buildMatrix(ratingsIn)
toRatingsMatrix(ratingsIn, NAval = NA)
toUserItemRatings(ratMat)
}

\arguments{
   \item{ratingsIn}{Raw data, in (user ID, item ID, rating) format.}
   \item{ratMat}{Ratings matrix}
   \item{NAval}{value to place in the output matrix for missings.}
}
\details{
The \code{buildMatrix} function inputs a user ratings matrix, and
returns the ratings matrix, with element (i,j) containing the rating of
item j by user i.

The \code{toRatingsMatrix} function is similar, but requires that user
and item IDs be factors, which are used to produce row names in the
output matrix.

The \code{toUserItemRatings} function converts in the opposite
direction.
}

\author{
	Pooja Rajkumar, Norm Matloff
}

