explore <- function(ratingsIn) {
  users <- ratingsIn[,1]
  items <- ratingsIn[,2]
  ratings <- ratingsIn[,3]
  Yi. <- tapply(ratings,users,mean) # means of all ratings per user
  Y.j <- tapply(ratings,items,mean) # means of all ratings per item
  Ni. <- tapply(ratings,users,length) # number of ratings per user
  N.j <- tapply(ratings,users,length) # number of ratings per item
  # histograms
  hist(ratings)
  par(ask=TRUE)
  hist(Yi.)
  hist(Y.j)
  hist(Ni.)
  hist(N.j)
  
  # scatterplots
  plot(sapply(users, function(x) Yi.[x]), ratings, main="Yi. vs Yij", xlab="Yi.", ylab="Yij")
  plot(sapply(items, function(x) Y.j[x]), ratings, main="Y.j vs Yij", xlab="Y.j", ylab="Yij")
}