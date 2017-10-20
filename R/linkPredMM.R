
# WORK IN PROGRESS!

# rec systs used for link prediction; adapted from findYdotsMM()

# undirected graph

# model: probability of link (i,j) = mu + alpha_i + alpha_j

# maybe run the latter throug logit, i.e. logit(a+b(alpha+alpha))?

#   ratingsIn: input data, with cols (fromVertex,toVertex),
#              vertex numbering 1,2,...

# value:

#   S3 class of type "ydots", with components:

#      Y..: grand mean, est of mu 
#      Yi.: vector of mean "ratings for each user, ests. of alpha_i
#      Y.j: vector of mean "ratings for each item, ests. of betaa_j
#      regObj: if have covariates, regression output, e.g. coefs

linkPredMM <- function(ratingsIn,numVerts) {
  fromV <- ratingsIn[,1]
  toV <- ratingsIn[,2]
  ratings <- ratingsIn[,3]
  nr <- nrow(ratingsIn)
  Y.. <- nr / numVerts^2
  Yi. <- tapply(ratings,fromV,length) / numVerts
  ydots <- list(grandMean=Y..,usrMeans=Yi.,itmMeans=Y.j)
  haveCovs <- ncol(ratingsIn) > 3
  if (haveCovs) {
     # center the covs
     tmp <- scale(ratingsIn[,-(1:3)],scale=FALSE)
     ratingsIn[,-(1:3)] <- tmp
     ydots$covmeans <- attr(tmp,'scaled:center')
     # regress, no constant term; could do a weighted least squares,
     # using the Ni, but since the latter is random too, not needed
     frml <- as.formula(paste(nms[3],'~ .-1'))
     lmout <- lm(frml,data=ratingsIn[,-(1:2)])
     ydots$lmout <- lmout
     Ni <- tapply(ratings,users,length) # number of ratings per user
     ydots$Ni <- Ni
  } 
  class(ydots) = 'ydotsMM'
  invisible(ydots)
}

# alias
trainMM <- findYdotsMM 

