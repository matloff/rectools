
###################    covnmf()   ##############################

# applies covariate information to NMF

# arguments:

#    narrowrat: 3-column df in (userID, itemID, rating) format, 
#       plus covariate columns, if any
#    k: desired rank in factorization

# value:

#    full predicted ratings matrix; later, object of class 'fullrat'

# if already have the predictive vals, call getnmf() directory

covNMF <- function(narrowrat,k) {
   nc <- ncol(narrowrat)
   if (nc > 3) {
      narrowrat[,3] <- narrowrat[,3] - getpreds(narrowrat)
      narrowrat <- pmax(narrowrat,0)
   }
   getNMF(narrowrat,k)
}

getPreds <- function(narrowrat) {
      nrw <- as.matrix(narrowrat)
      tmp <- lm(nrw[,3],nrw[,4:nc])
      predict(tmp,nrw[,4:nc])
}

###################    getnmf()   ##############################

# uses NMF package, probably should change, as it is intolerant of rows
# of 0s

getNMF <- function(narrowrat,k) {
   require(NMF)
   a <- buildMatrix(narrowrat,0)  # 0s for missing entries
   nmfout <- nmf(a,k)
   w <- nmfout@fit@W
   h <- nmfout@fit@H
   w %*% h
}

###################    getnmf()   ##############################

# say have matrix A to complete, resulting in A-hat, with estimated
# entries; this evaluates the predictive ability, with 'rats' being the
# (useID, itemID, rating) matrix

predahat <- function(ahat,rats) {
   nrats <- nrow(rats)
   preds <- vector(length = nrats)
   for (i in 1:nrats) {
      preds[i] <- ahat[rats[i,1],rats[i,2]]
   }
   actual <- rats[,3]
   mean(abs(round(preds) - actual))
}

