trainReco <- function(ratingsIn,rnk = 10)
 {
   library(recosystem)
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

predict.RecoS3 <- function(recoObj,testSet) {
   p = recoObj$P
   q = recoObj$Q
   testSet$pred <- vector(length=nrow(testSet))
   for(i in 1:nrow(testSet)){
      j = testSet[i,1]
      k = testSet[i,2]
      if(j < nrow(p) || k < nrow(q)) #NA else
         testSet$pred[i] <- p[j,] %*% q[k,]
      else
         testSet$pred[i] <- NA
   }
   testSet$pred
}

