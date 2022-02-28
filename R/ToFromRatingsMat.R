
# form ratings matrix from the usual (user ID, item ID, rating) data

toRatingsMatrix <- function(ratingsIn,NAval=NA){
   # will now require that the IDs be factors
   
   users = ratingsIn[,1]
   items = ratingsIn[,2]
   if(!is.factor(users) || !is.factor(items)) 
      stop('user and item IDs must be R factors')
   ratings = ratingsIn[,3]
   newMatrix = matrix(NAval, 
          nrow = length(levels(users)), ncol = length(levels(items)))
   levelsU <- levels(users)
   levelsI <- levels(items)
   rownames(newMatrix) <- levelsU
   colnames(newMatrix) <- levelsI
   users <- as.character(users)
   items <- as.character(items)
   for(rowNum in 1:nrow(ratingsIn)) {
       newMatrix[users[rowNum],items[rowNum]] <- ratings[rowNum] 
   }
   newMatrix
}

buildMatrix <- function(ratingsIn,NAval=0){
   # deal with possible factors
   dmax <- function(d) {
      if (is.factor(d)) return(length(levels(d)))
      max(d)
   }
   users = ratingsIn[,1]
   movies = ratingsIn[,2]
   rating = ratingsIn[,3]
   newMatrix = matrix(NAval, 
          nrow = dmax(users), ncol = dmax(movies))
   for(rows in 1:nrow(ratingsIn)){
       newMatrix[ratingsIn[rows,1],ratingsIn[rows,2]] = ratingsIn[rows,3]
   }
   return (newMatrix)
}

# go the opposite way: convert ratings matrix to the usual (user ID,
# item ID, rating) data

# arguments:

#    ratMat:  ratings matrix, users in rows, items in cols, NAs where
#             unavailable 

# value:  the (user ID, # item ID, rating) data frame; user and item IDs
# come from row and column numbers in the ratings matrix


toUserItemRatings <- function(ratMat) 
{
   # might use various reshape versions, but making it explicit will
   # allow extensions

   n <- nrow(ratMat)
   m <- ncol(ratMat)
   outDF <- NULL

   for (i in 1:n) {
      rw <- ratMat[i,]
      nonNA <- which(!is.na(rw))
      nNonNA <- length(nonNA)
      if (nNonNA > 0) {
         toAppend <- data.frame(
            userID=rep(i,nNonNA),
            itemID=nonNA,
            ratings=rw[nonNA])
         outDF <- rbind(outDF,toAppend)
      }
   }
   outDF
}

