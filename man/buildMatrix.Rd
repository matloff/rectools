
\name{buildMatrix}
\alias{buildMatrix}
\title{Build Ratings Matrix}

\description{
Reformats a rating matrix where each row represents a user id, item id, and rating into a rating matrix where rows represent users and columns represent items, with rating information at the intersecting data points.
}

\usage{
buildMatrix(ratingsIn)
}

\arguments{
	\item{ratingsIn}{Raw rating data, with rows in (user ID, item ID, rating) format.}
}

\details{
The \code{buildMatrix} function recieves a user ratings matrix with three columns representing (user, item, rating) as input, and returns the ratings matrix, with element (i,j) containing user i's rating of item j.

In exact terms - the result is a new matrix in which the rows, columns, and values are the \code{ratingsIn}'s first, second, and third columns respectively.

Depending on the \code{ratingsIn}, but more often than not, the resulting matrix will be very sparse, with many zero-based values, as most users will not have provided rating information for most items.
}

\value{
A new matrix where element (i, j) containing user i's rating of item j, given the first three columns of \code{ratingsIn}.
}

\examples{
## simple raw data, with ratings by user 1 on items 3 and 4
## and ratings by user 2 on item 1.
d <- rbind(c(1,3,8),c(2,1,3),c(1,4,5))

## resulting matrix will show user ratings at (1,3), (1,4), and (2,1)
buildMatrix(d)
}

\author{
	Pooja Rajkumar, Joseph Lewis (\email{joelewis@ucdavis.edu})
}
