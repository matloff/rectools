\name{cosine, predict.usrData}
\alias{cosine}
\alias{predict.usrData}

\title{Predict Using k-NearestNeighbor}

\description{
Calculates predicted ratings using nearest neighbors method (distance between given user ratings calculated via \code{cosDist} as the cosine, or inner product, distance). Can be improved upon using user demographics (covariates, e.g. age, gender) and item type preferences (e.g. preferred movie genres), whose influence weights are supplied as optional arguments if desired.
}

\usage{
  predict.usrData(origData, newData, newItem, k, 
                  wtcovs = NULL, wtcats = NULL)
}

\arguments{
  \item{origData}{the training set data (object \code{usrData}, output of \code{findUserData})}
  \item{newData}{the single data point to be predicted (later will be expanded to include multiple data points) (this is an object of class \code{usrDatum}, same class as each element of the list\code{origData}. The \code{userID} component can be \code{NA}}
  \item{newItem}{the id of the rating item to predict for the user specified in \code{newData}}
  \item{k}{the number of nearest neighbors to include in prediction calculations, interpretted as a vector}
  \item{wtcovs}{the weight to put on covariates; NULL if no covs.}
  \item{wtcats}{the weight to put on item categories; NULL if no cats.}
}

\details{
  Note that inner product distance modelling has some problems (e.g. its scale-free nature), even though it is the standard method used for k-nearest-neighbor modelling.  Other choices for distance measurement will be added in the future to provide alternatives which address these problems.
}

\value{
  Predicted rating for \code{newData}.
}

\author{
	Vishal Chakraborty, Joseph Lewis (\email{joelewis@ucdavis.edu}), and Norm Matloff
}

\examples{
     ivl <- InstEval 
     ivl$s <- as.numeric(ivl$s) 
     ivl$d <- as.numeric(ivl$d) 
     ivl <- ivl[,c(1,2,7)]
     usrdata <- formUserData(ivl[,1:3]) 
     # predict the rating user 3 would give item 8
     predict(usrdata,usrdata[[3]],8,10)  # 2.6
}
