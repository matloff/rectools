
########################## predict() function ##########################
#' @title predict newData argument from origData argument
#'
#' Calculates predicted ratings using nearest neighbors methods using 
#' cosDist (the cosine, or inner product, distance) between user 
#' ratings that are given. Can be improved upon using user covariate 
#' information (demographics, e.g. age, gender) and item category type 
#' preferences (e.g. movie genres), whose influence weights are supplied
#' as optional arguments if desired.
#'
#' @param origData: the training set data (object of class 'usrData' - see file
#'                  findUsrItemData.R)
#' @param newData: the data point to be predicted (just a single datum for now, 
#'                 object of class 'usrDatum')
#' @param newItem: the id of the rating item to predict for the user specified 
#'                 in newData
#' @param k: the number of nearest neighbors to include in prediction
#' @param wtcovs: the weight to put on covariates; NULL if no covs
#' @param wtcats: the weight to put on item categories; NULL if no cats
#'
#' @return the predicted ratings for newData

predict.usrData <- function(origData, newData, newItem, k, 
                            wtcovs=NULL, wtcats=NULL) {
  
  # start by narrowing origData down to users who have rated the identified item
  
  # checkNewItem(oneUser) : take a single user record (oneUser) from origData
  #                        and search for a j such that element j in
  #                        in the items list for oneUsr matches newItem.
  #   if such a j exists, then (j, rating) will be returned,
  #   otherwise, (NA, NA) will be returned.
  # This is defined for use in sapply upon origData, to narrow it down
  checkNewItem <- function(oneUser) {
    tmp <- match(oneUser$itms, newItem)
    if(all(is.na(tmp))) {
      c(NA, NA)
    } else {
      whichOne <- which(!is.na(tmp))
      c(whichOne, oneUser$ratings[whichOne])
    }
  }
  found <- as.matrix(sapply(origData, checkNewItem))
  
  # found is of dimensions: 2, number of users
  # found[1,i] = j means origData[[i]]$itms[j] = newItem
  # found[1,i] = NA means newItem wasn't rated by user i
  # found[2,i] will be the rating in the non-NA case
  
  # we need to get rid of the NA users
  ratedUserIndexes <- which(!is.na(found[1,]))
  # ratedUserIndexes[i] is the index, i.e. user ID, 
  #                     of the i-th user who has rated newData
   
  # we cannot perform nearest neighbor predictions about this item's rating
  # for a given user if no other users have rated it
  if (is.null(ratedUserIndexes) | length(ratedUserIndexes) == 0) {
    return(NA) # results are non-applicable to an unrated item
  }

  # remove all non-relavent users from origData, leaving
  # only the ones who have rated the item in question
  origData <- origData[ratedUserIndexes]
  
  # remove the na items from the found matrix
  found <- found[,ratedUserIndexes,drop=FALSE]

  # oneUserCosDist(oneUser) : distance from newData to oneUser of origData 
  #   uses cosine (inner product) distance for calculation
  # defined for use in sapply() below
  oneUserCosDist <- function(oneUser) cosDist(newData,oneUser,wtcovs,wtcats)
  
  # compile the distances from newData to all original data points in a vector
  cosines <- sapply(origData,oneUserCosDist)

  # findKNeighborRating : predict rating of user based on each of k[i] neighbors
  #                       uses the maximum number of neighbors available if
  #                       any k[i] is larger than the number of possible
  #                       neighbors, sorted by distance (nearest first)
  # used with the sapply to find the expected rating by mean of predictions
  findKNeighborRating <- function(count) {
    count <- min(count, length(cosines))
    klarge <- order(cosines, decreasing=TRUE)[1:count]
    mean(as.numeric(found[2, klarge]))
  }
  
  # the nearest neighbor rating mean for all k, i.e. our prediction
  sapply(k, findKNeighborRating)
}

#'find cosine distance between x and y, elements of an object
#' of 'usrData' class
#'
#'  \code{cosDist} find cosine distance between x and y, elements of an object
#'   of 'usrData' class; only items rated in both x and y are used; if none
#'   exist, then return NaN
#'   @x: object of usrData class
#'   @y: second object of usrData class
#'  @wtcovs: weight to put on covariates; NULL if no covs
#'  @wtcats: weight to put on item categories; NULL if no cats
cosDist <- function(x,y,wtcovs,wtcats) {
  # rated items in common
   commItms <- intersect(x$itms,y$itms)
   if (is.null(commItms)| length(commItms)==0) return(NaN)
   # where are they in x and y?
   xwhere <- which(!is.na(match(x$itms,commItms)))
   ywhere <- which(!is.na(match(y$itms,commItms)))
   xrats <- x$ratings[xwhere]
   yrats <- y$ratings[ywhere]
   cosTot <- xrats %*% yrats
   xl2 <- sum(xrats^2)
   yl2 <- sum(yrats^2)
   if (!is.null(wtcovs)) {
      cosTot <- cosTot + wtcovs * x$cvrs %*% y$cvrs
      xl2 <- xl2 + sum((wtcovs*x$cvrs)^2)
      yl2 <- yl2 + sum((wtcovs*y$cvrs)^2)
   }
   if (!is.null(wtcats)) {
      cosTot <- cosTot + wtcats * x$cats %*% t(y$cats)
      xl2 <- xl2 + sum((wtcovs*x$cats)^2)
      yl2 <- yl2 + sum((wtcovs*y$cats)^2)
   }
   cosTot / sqrt(xl2 * yl2)
}
