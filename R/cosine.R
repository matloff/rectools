
# nearest neighbor prediction via "cosine" method:

# the user has already called formUserData() on the training set, and
# now wishes to predict a new case; the question is, what rating would
# the user newData give to item newItem?

# first, the cases in the training set for which a rating for newItem is
# available are found; then each such case is compared to newData, by
# computing the "cosine" between the training case and newData

# the "cosine" between two cases A and B is defined according to the
# items rated in common between them; the dot product (in the linear
# algebra sense) of the ratings of A and the ratings of B, among the
# items in common, then divided by the product of the norms of those two
# vectors

# we then find the prediction by averaging the ratings of newItem among
# the k training cases having the highest cosine with newData 

# covariates (e.g. age, gender) and item type preferences (e.g.
# preferred movie genres) are allowed in distance computation for
# neighbors; the covariates are assumed here to be user covariates, as
# this is what findUsrItmData.R gives

# NOTE:  the cosine, though standard, has certain problems; e.g., its
# scale-free nature means two users are very "close" even if, say, one 
# is almost exactly double the other; other choices for distance measure
# will be added

# arguments:

#    origData: training set, object of class 'usrData', a list of
#              objects of class 'usrDatum' (see file findUsrItmData.R)
#    newData: data point to be predicted, object of class 'usrDatum'; 
#             just a single point here, but have predictUsrDataMany()
#             later below
#    newItem: ID of the item rating to be predicted for the 
#             user in newData
#    wtcovs: weight to put on covariates; NULL if no covs
#    wtcats: weight to put on item categories; NULL if no cats
#    k: a vector of the numbers of nearest neigbhors used for
#       predicting; one predicted rating will be calculated for
#       each k

# value:

#    predicted ratings for newData for the item newItem

predict.usrData <- function(origData,newData,newItem,
      k,wtcovs=NULL,wtcats=NULL) 
{
   # check that newData has the covs and cats if and only if the
   # training data did
   traincovs <- !is.null(origData[[1]]$cvrs)
   newcovs <- !is.null(newData$cvrs)
   if (!(traincovs + newcovs %in% c(0,2)))
      stop('mismatch in having/not having covars, orig and new data')
   traincats <- !is.null(origData$cats)
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
   checkNewItem <- function(oneUsr) {
      whichOne <- which(oneUsr$itms == newItem)
      if (length(whichOne) > 1) {
         stop("same user/item pair encountered more than once")
      }
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
   if (is.null(whoHasIt) | length(whoHasIt) == 0) 
      return(NA)  # no one rated this item
   origDataRatedNI <- origData[whoHasIt]
   # now origDataRatedNI only has the relevant users, the ones who 
   # have rated newItem, so select only those columns of the found matrix
   found <- found[,whoHasIt,drop=FALSE]

   # find the distance from newData to one user y of origData; defined for
   # use in sapply() below
   onecos <- function(y) cosDist(newData,y,wtcovs,wtcats)
   cosines <- sapply(origDataRatedNI,onecos)
   # the vector cosines contains the distances from newData to all the
   # original data points who rated newItem

   # action of findKnghbourRtng(): find the mean rating of newItem in
   # origDataRatedNI, for ki (= k[i]) neighbors
   #
   # if ki > neighbours present in the dataset, then the 
   # number of neighbours is used
   findKnghbourRtng <- function(ki){
     ki <- min(ki, length(cosines))
     # nearby is a vector containing the indices of the ki 
     # most similar neighbours
     nearby <- order(cosines,decreasing=TRUE)[1:ki]
     mean(as.numeric(found[2, nearby]))
   }
   sapply(k, findKnghbourRtng)
}

# find cosine distance between x and y, objects
# of 'usrData' class
#
# only items rated in both x and y are used; if none
# exist, then return NaN
#
#  wtcovs: weight to put on covariates; NULL if no covs
#  wtcats: weight to put on item categories; NULL if no cats

cosDist <- function(x,y,wtcovs=NULL,wtcats=NULL) 
{
   # rated items in common
   commItms <- intersect(x$itms,y$itms)
   if (length(commItms)==0) return(NaN)
   # where are those common items in x and y?
   xwhere <- which(!is.na(match(x$itms,commItms)))
   ywhere <- which(!is.na(match(y$itms,commItms)))
   xvec <- x$ratings[xwhere]
   yvec <- y$ratings[ywhere]
   if (!is.null(wtcovs)) {
      xvec <- c(xvec,wtcovs*x$cvrs)
      yvec <- c(yvec,wtcovs*y$cvrs)
   }
   if (!is.null(wtcats)) {
      xvec <- c(xvec,wtcats*x$cats)
      yvec <- c(yvec,wtcats*y$cats)
   }

   xvec %*% yvec / (l2a(xvec) * l2a(yvec))
}

l2a <- function(x) sqrt(x %*% x)

testCos <- function() 
{
    rts <- rbind(c(1, 3, 5), c(4, 2, 2), c(4, 1, 2), c(5, 6,2), 
       c(1, 6, 5),c(1,2,1),c(1,1,5))
    ud <- formUserData(rts)
    print(predict(ud,ud[['r']],3,1))  # should print 5
}

# temporary solution to the problem of predict.usrData predicting only
# one data point at a time

#  origData,k,wtcovs,wtcovs:  see predict.usrData() above
#  newDataMany: a vector of user IDs (must have an entry in origData)
#  newItemMany: a vector of item IDs (must be in origData)

predictUsrDataMany <- function(origData,newDataMany,newItemMany,
      k,wtcovs=NULL,wtcats=NULL) 
{
   nNew <- length(newDataMany)
   preds <- vector(length = nNew)
   for (i in 1:nNew) {
      origDataEntry <- origData[[as.character(newDataMany[i])]]
      preds[i] <- predict.usrData(origData,origDataEntry,newItemMany[i],k=k)
   }
   preds
}

