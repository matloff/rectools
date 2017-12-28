

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
trainRecoPar <- function(ratingsIn,rnk = 10, cls)
{
   require(recosystem)
   require(partools)
   clusterEvalQ(cls,require(recosystem))
   clusterdistribsplit(cls,'ratingsIn')
print('must add code to add token records for users/items in at least one chunk   but not others') 
   clusterExport(cls,c('rnk'),envir=environment())
   res <- clusterEvalQ(cls,
      {
      r <- Reco()
      train_set <- 
         data_memory(ratingsIn[,1],ratingsIn[,2],ratingsIn[,3],index1=TRUE)
      r$train(train_set,opts = list(dim=rnk)) 
      P_file = out_file(tempfile())
      Q_file = out_file(tempfile())
      res = r$output(out_memory(),out_memory())
      }
   )
   print('add code to average the PQ products over the cluster nodes')
   result <- list(P = res$P, Q = res$Q)
   class(result) <- 'RecoS3'
   result
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

