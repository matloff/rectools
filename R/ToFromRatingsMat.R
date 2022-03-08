
# form ratings matrix from the usual (user ID, item ID, rating) data

toRatingsMatrix <- function(ratingsIn,NAval=NA){

   # will now require that the IDs be factors
   
   users = ratingsIn[,1]
   items = ratingsIn[,2]
   if(!is.factor(users) || !is.factor(items)) 
      stop('user and item IDs must be R factors')
   ratings = ratingsIn[,3]
   users <- toTrueLevels(users)
   items <- toTrueLevels(items)
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

# factor f may be a subset of an original; make a new factor only with
# the levels in f

toTrueLevels <- function(f) 
{
   fChar <- as.character(f)
   as.factor(fChar)
}

buildMatrix <- function(ratingsIn,NAval=0){
   stop('use toUserItemRatings()')
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
#       unavailable; must have row and column names 

# value:  the (user ID, # item ID, rating) data frame; user and item IDs
# come from row and column names in the ratings matrix

toUserItemRatings <- function(ratMat) 
{
   # might use various reshape versions, but making it explicit will
   # allow extensions

   n <- nrow(ratMat)
   m <- ncol(ratMat)
   outDF <- NULL

   # determine row, col names; create them if they're not there
   rns <- rownames(ratMat)
   cns <- colnames(ratMat) 

   if(length(rns) == 0 || !length((cns))) 
      stop('input must have row and column names')

   # unsF <- as.factor(uns)
   # insF <- as.factor(ins)
   # unsL <- levels(unsF)
   # insL <- levels(insF)

   for (i in 1:n) {
      rw <- as.numeric(ratMat[i,])
      nonNA <- which(!is.na(rw))
      nNonNA <- length(nonNA)
      if (nNonNA > 0) {
         toAppend <- data.frame(
            userID=rep(rns[i],nNonNA),
            itemID=cns[nonNA],
            ratings=rw[nonNA])
         outDF <- rbind(outDF,toAppend)
      }
   }

   outDF[,1] <- as.factor(outDF[,1])
   outDF[,2] <- as.factor(outDF[,2])

   outDF
}

touir <- toUserItemRatings

