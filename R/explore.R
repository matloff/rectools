# generates a sequence of plots exploring input data

# arguments:

#   ratingsIn: input data, with first cols (userID,itemID,rating,
#              covariates)
#   plotcovs: if True, generate plots for covariates (if there are any)

explore <- function(ratingsIn, plotcovs=TRUE) {
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
  userMeans <- sapply(users, function(x) Yi.[x])
  itemMeans <- sapply(items, function(x) Y.j[x])
  
  plot(userMeans, ratings, main="Yi. vs Yij", xlab="Yi.", ylab="Yij")
  plot(itemMeans, ratings, main="Y.j vs Yij", xlab="Y.j", ylab="Yij")
  
  # heatmap
  palette <- colorRampPalette(c('blue','red'))
  ratColors <- palette(10)[as.numeric(cut(ratings, breaks=10))]
  plot(userMeans, itemMeans, col=ratColors, main="Ratings Heatmap", xlab="Yi.", ylab="Y.j")
  
  # if there are covariates
  if (plotcovs && ncol(ratingsIn) > 3) {
    for (i in 4:ncol(ratingsIn)) {  # plot a histogram/barchart for each covariate column
      covName = colnames(ratingsIn)[i]
      
      if (is.numeric(ratingsIn[,i])) {
        hist(ratingsIn[,i], main=paste("Histogram of",covName), xlab=colnames(ratingsIn)[i])
      }
      else {
        barplot(table(ratingsIn[,i]), main=paste("Bar plot of", covName), xlab=colnames(ratingsIn)[i])
      }
    }
  }
}