
buildMatrix <- function(ratingsIn,NAval=0) {

  # sub-routine to deal with possible factors
  dmax <- function(d) {
  
    if (is.factor(d)) {
      return(length(levels(d)))
    }
  
    return(max(d))
  }
  
  # begin formulating the new matrix  
  users = ratingsIn[,1]
  items = ratingsIn[,2]
  ratings = ratingsIn[,3]
 
  newMatrix = matrix(NAval, nrow = dmax(users), ncol = dmax(items))
 
  # populate the matrix with the given user ratings
  for(row in 1:nrow(ratingsIn)) {
    newMatrix[users[row],items[row]] = ratings[row]
  }
 
  return(newMatrix)
}
