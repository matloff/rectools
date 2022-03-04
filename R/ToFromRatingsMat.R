
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
#       unavailable 
#    rowColNames:  if TRUE, use row, col names of ratMat in output 
#       userID, itemID columns; else, use row, col numbers in these
#       output columns

# value:  the (user ID, # item ID, rating) data frame; user and item IDs
# come from row and column numbers in the ratings matrix

toUserItemRatings <- function(ratMat,rowColNames=FALSE) 
{
   # might use various reshape versions, but making it explicit will
   # allow extensions

   n <- nrow(ratMat)
   m <- ncol(ratMat)
   outDF <- NULL

   # determine row, col names; create them if they're not there
   rns <- rownames(ratMat)
   cns <- colnames(ratMat) 
   if (length(rns) == 0) {
      inputHasRownames <- FALSE
      if (rowColNames) rns <- 1:n
   } else inputHasRownames <- TRUE
   if (length(cns) == 0) {
      inputHasColnames <- FALSE
      if (rowColNames) cns <- 1:m
   } else inputHasColnames <- TRUE

   rnsF <- as.factor(rns)
   cnsF <- as.factor(cns)
   rnsL <- levels(rnsF)
   cnsL <- levels(cnsF)

   for (i in 1:n) {
      rw <- ratMat[i,]
      nonNA <- which(!is.na(rw))
      nNonNA <- length(nonNA)
      if (nNonNA > 0) {
         if (rowColNames) {
            toAppend <- data.frame(
               userID=rep(rnsL[i],nNonNA),
               itemID=cnsL[nonNA],
               ratings=rw[nonNA])
         } else {
            toAppend <- data.frame(
               userID=rep(i,nNonNA),
               itemID=nonNA,
               ratings=rw[nonNA])
         }
         outDF <- rbind(outDF,toAppend)
      }
   }

   outDF
}

touir <- toUserItemRatings

