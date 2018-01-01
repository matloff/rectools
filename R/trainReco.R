

trainReco <- function(ratingsIn,rnk = 10)
 {
   require(recosystem)
   r <- Reco()
   train_set <- 
      data_memory(ratingsIn[,1],ratingsIn[,2],ratingsIn[,3],index1=TRUE)
   r$train(train_set,opts = list(dim=rnk)) 
   P_file = out_file(tempfile())
   Q_file = out_file(tempfile())
   res = r$output(out_memory(),out_memory())
   result <- list(P = res$P, Q = res$Q)
   class(result) <- 'RecoS3'
   result
 }

# Software Alchemy version of trainReco()
trainRecoPar <- function(ratingsIn,rnk = 10, cls) {
   require(recosystem)
   require(partools)
   clusterEvalQ(cls,require(recosystem))
   clusterdistribsplit(cls,'ratingsIn')
   clusterExport(cls,'rnk',envir=environment())
   clusterEvalQ(cls,r <- Reco())
   clusterEvalQ(cls,train_set <- 
      data_memory(ratingsIn[,1],ratingsIn[,2],ratingsIn[,3],index1=TRUE))

   # need to account for some users being in some chunks but not others,
   # and same for items; add fake rows and cols consisting of a single 1
   # rating
   tmp <- clusterEvalQ(cls,users <- unique(ratingsIn[,1]))
   allUsers <- unique(unlist(tmp))
   clusterExport(cls,c('allUsers'),envir=environment())
   tmp <- clusterEvalQ(cls,items <- unique(ratingsIn[,2]))
   allItems <- unique(unlist(tmp))
   clusterExport(cls,c('allItems'),envir=environment())
   clusterEvalQ(cls,
     {
        for (usr in allUsers) {
           if (!(usr %in% ratingsIn[,1])) 
              ratingsIn <<- rbind(c(usr,1,1))
        };
        for (itm in allItems) {
           if (!(itm %in% ratingsIn[,2])) 
              ratingsIn <<- rbind(c(1,itm,,1))
        }
     })

   # now compute the factorizations
   result <- clusterEvalQ(cls,
      {
      r$train(train_set,opts = list(dim=rnk)); 
      P_file = out_file(tempfile());
      Q_file = out_file(tempfile());
      res = r$output(out_memory(),out_memory())
      result <- list(P = res$P, Q = res$Q)
      result
      })
   class(result) <- 'RecoS3par'
}

predict.RecoS3 <- function(recoObj,testSet) {
   p = recoObj$P  # transpose of classic W
   q = recoObj$Q  # classic H
   testSet$pred <- vector(length=nrow(testSet))
   for(i in 1:nrow(testSet)){
      j = testSet[i,1]
      k = testSet[i,2]
      # is user or item not in the dataset?; if so, NA
      if(j < nrow(p) || k < nrow(q)) 
         testSet$pred[i] <- p[j,] %*% q[k,]
      else
         testSet$pred[i] <- NA
   }
   testSet$pred
}

# predict.RecoS3par <- 

