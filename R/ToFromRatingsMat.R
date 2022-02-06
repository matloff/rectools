
# form ratings matrix from the usual (user ID, item ID, rating) data

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

#    idCol:  input row ID
#    ratMat:  ratings matrix, users in rows, items in cols, NAs where
#             unavailable 
#    valCols:  values to make 'long'; column numbers or column names
#    sideCols:  side information; column numbers or column names

ToUserItemRatings <- function(ID,ratMat,valCols,sideCols) 
{
   # could use various reshape versions, but making it explicit will
   # allow extensions

   doOneRow <- function(rw)   # do one row of the input data
   {
      id <- rw[1,idCol]
      vals <- rw[,valCols]
      side <- rw[,sideCols]
   }

}
