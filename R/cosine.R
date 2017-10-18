
# nearest neighbor prediction via "cosine" method

# covariates (e.g. age, gender) and item type preferences (e.g.
# preferred movie genres) are allowed in distance computation for
# neighbors

# note:  the cosine, though standard, has certain problems; e.g., its
# scale-free nature means two users are very "close" even if one is
# almost exactly double the other; other choices for distance measure
# will be added

# arguments:

#    origData: training set, object of class 'usrData', a list of
#              objects of class 'usrDatum' (see file findUsrItmData.R)
#    newData: data point (just one in current code) to be predicted, 
#             object of class 'usrDatum'
#    newItem: ID of the item rating to be predicted for the 
#             user in newData
#    wtcovs: weight to put on covariates; NULL if no covs
#    wtcats: weight to put on item categories; NULL if no cats
#    k: a vector of the numbers of nearest neigbhors used for
#       predicting; one predicted rating will be calculated for
#       each k

# value:

#    predicted ratings for newData


predict.usrData <- function(origData,newData,newItem,
      k,wtcovs=NULL,wtcats=NULL) {

   # check that newData has the covs and cats if and only if the
   # training data did
   traincovs <- !is.null(origData$usrCovs)
   newcovs <- !is.null(newData$cvrs)
   if (!(traincovs + newcovs %in% c(0,2)))
      stop('mismatch in having/not having covars, orig and new data')
   traincats <- !is.null(origData$usrCats)
   newcats <- !is.null(newData$cats)
   if (!(traincats + newcats %in% c(0,2)))
      stop('mismatch in having/not having cats, orig and new data')

   # we first need to narrow origData down to the users who 
   # have rated newItem
   
   # action of checkNewItem(): here oneUsr is one user record in
   # origData; the function will look for a j such that element j in the
   # items list for this user matches the item of interest, newItem; if
   # such a j exists, then (j,rating) will be returned, otherwise
   # (NA,NA); defined for use by sapply() below
   ###   checkNewItem <- function(oneUsr) {
   ###     tmp <- match(oneUsr$itms, newItem)
   ###     if (all(is.na(tmp))) {
   ###       c(NA,NA)
   ###     }
   ###     else{
   ###       whichOne <- which(!is.na(tmp))
   ###       c(whichOne,oneUsr$ratings[whichOne])
   ###     }
   ###   }
   # NM refactor, 10/18/17
   checkNewItem <- function(oneUsr) {
      whichOne <- which(oneUsr$itms == newItem)
      if (length(whichOne) == 0) {
         return(c(NA,NA))
      }
      else return(c(whichOne,oneUsr$ratings[whichOne]))
   }

   found <- as.matrix(sapply(origData,checkNewItem))
   # description of 'found':
   # found is of dimensions 2 x number of users in training set
   # found[1,i] = j means origData[[i]]$itms[j] = newItem;
   # found[1,i] = NA means newItem wasn't rated by user i
   # found[2,i] = rating in the non-NA case
   
   # we need to get rid of the users who didn't rate newItem
   whoHasIt <- which(!is.na(found[1,]))
   # whoHasIt[i] is the index, i.e. user ID, of the i-th user who has
   # rated newData
   if (is.null(whoHasIt) | length(whoHasIt) == 0) 
      return(NA)  # no one rated this item
   origData <- origData[whoHasIt]
   # now origData only has the relevant users, the ones who have rated
   # newItem, so select only those columns of the found matrix
   found <- found[,whoHasIt,drop=FALSE]

   # find the distance from newData to one user y of origData; defined for
   # use in sapply() below
   onecos <- function(y) cosDist(newData,y,wtcovs,wtcats)
   cosines <- sapply(origData,onecos)
   # the vector cosines contains the distances from newData to all the
   # original data points

   # action of findKnghbourRtng(): predict rating based on each k[i] neighbours
   # x = k[i]
   # if x > neighbours present in the dataset, then the maximum 
   # number of neighbours is used
   findKnghbourRtng <- function(x){
     # x can be at most the number of neighbours in the dataset
     x <- min(x, length(cosines))
     # klarge is a vector containing the indices of the x closest neighbours
     klarge <- order(cosines,decreasing=TRUE)[1:x]
     mean(as.numeric(found[2, klarge]))
   }
   sapply(k, findKnghbourRtng)
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
