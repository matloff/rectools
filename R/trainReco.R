
# training and prediction routines, wrappers for such operations in the
# 'recosytem' package, including some parallel operations

# 'recosytem' approximates the (mostly unknown) user-ratings matrix A as
# the product P'Q, with P and Q having specified rank 'rnk'; a number of
# other tuning parameters are available besides rank, but not used here

# it is assumed that user and item IDs are contiguous, starting at 1

#############################  trainReco  ***************************

# applies 'recosystem' to a training set

# arguments:

#    ratingsIn:  raw input matrix, with cols usrID, itmID, rating 
#    rnk:  desired rank for the P,Q matrices
#    nmf:  if true, use NMF, otherwise SVD

# value:  object of class 'RecoS3', a list containing P and Q

trainReco <- function(ratingsIn,rnk=10,nmf=FALSE)
{
   require(recosystem)
   r <- Reco()
   train_set <- 
      data_memory(ratingsIn[,1],ratingsIn[,2],ratingsIn[,3],index1=TRUE)
   r$train(train_set,opts = list(dim=rnk,nmf=nmf)) 
   result <- r$output(out_memory(),out_memory())
   class(result) <- 'RecoS3'
   result
}

##########################  trainRecoPar  ***************************

# Software Alchemy/partools version of trainReco()

# we could find the P and Q matrices at the worker nodes, then average
# their P'Q products at the manager, to get an overall P'Q (we could NOT
# simply average the Ps and average the Qs, as they are not unique); but
# it is assumed here that the full P'Q matrix may be too large to store at
# the manager 

#instead, breaks data into chunks, each of which is handled by a worker
#node, and applies Reco; note that the code follows the partools "leave
#it there" philosophy, retaining the P and Q matrices rather than
#returning them to the manager node


# arguments:

#    ratingsIn: raw input matrix
#    rnk:  desired rank from P,Q matrices
#    cls:  a 'partools' cluster
#    pqName:  name to be given to the list containing P,Q matrices
#             at the worker nodes

# value:  object of class 'RecoS3par', consisting of pqName

trainRecoPar <- 
   function(ratingsIn,rnk=10,nmf=FALSE,cls,pqName='PQ',printTimes=TRUE) 
{
   require(recosystem)
   require(partools)
   clusterEvalQ(cls,require(recosystem))
   tmp <- system.time(
      distribsplit(cls,'ratingsIn')
   )
   if (printTimes) cat('distribsplit time: ',tmp,'\n')
   clusterExport(cls,c('rnk','nmf','pqName','predict.RecoS3'),
      envir=environment())

   # note the possibility of some users being in some chunks but not others,
   # and same for items; at each node: we could add "fake" user and/or
   # item records (see commented code at the end of this file), but
   # since we are just leaving P and Q at the worker nodes and then
   # later averaging the nodes' predictions, no need for that here

   # now compute the factorizations at each node
   result <- clusterEvalQ(cls,
      {
      r <- Reco()
      train_set <- 
         data_memory(ratingsIn[,1],ratingsIn[,2],ratingsIn[,3],index1=TRUE)
      r$train(train_set,opts = list(dim=rnk,nmf=nmf))
      res <- r$output(out_memory(),out_memory())
      assign(pqName,res,envir = .GlobalEnv)
      result <- pqName
      class(result) <- 'RecoS3par'
      result
      })
   result[[1]]
}

##########################  predict.RecoS3  ***************************

# predicts from outputs of applying Reco to a training set

# note:  recosystem also has a predict() function, but it is not used

# recoObj is output of trainReco(), an object of class 'RecoS3; testSet
# is a 3-column raw data matrix as with ratingsIn above; returns the
# predicted values

predict.RecoS3 <- function(recoObj,testSet) 
{
   p <- recoObj$P  # transpose of classic W
   q <- recoObj$Q  # classic H
   testSet$pred <- vector(length=nrow(testSet))
   for(i in 1:nrow(testSet)){
      j <- testSet[i,1]
      k <- testSet[i,2]
      # is user or item not in the dataset?; if so, NA
      ## if(j < nrow(p) || k < nrow(q)) 
      if(j <= nrow(p) && k <= nrow(q)) 
         testSet$pred[i] <- p[j,] %*% q[k,]
      else
         testSet$pred[i] <- NA
   }
   testSet$pred
}

#######################  predict.RecoS3Par  ***************************

# predicts using Reco, like predict.RecoS3(), but in a distributed
# manner:  for any given case, predicts that case at each worker node, 

predict.RecoS3par <- function(RecoS3parObj,testSet,cls) 
{
   clusterExport(cls,c('RecoS3parObj','predict.RecoS3','testSet'),
      envir=environment())
   # prep to call predict.RecoS3() at each worker node
   clusterEvalQ(cls,pq <- get(RecoS3parObj))
   clusterEvalQ(cls,class(pq) <- 'RecoS3')
   # now, do the prediction; some will return NA, due to a missing user
   # and item at one of the nodes
   preds <- clusterEvalQ(cls,pred <- predict(pq,testSet))
   # now average them 
   predmatrix <- matrix(unlist(preds),ncol=nrow(testSet),byrow=TRUE)
   colMeans(predmatrix,na.rm=TRUE)
}

testTrainPredict <- function() 
{
   # the 2 sets of predictions should be within about 5% of each other
   cls <- makeCluster(2)
   setclsinfo(cls)
   getML100K(datadir='~/Research/DataSets/MovieLens/ML100K/ml-100k')
   set.seed(99999)
   testSet <- uduu[sample(1:nrow(uduu),250),]
   mlout <- trainReco(uduu,rnk=75)
   predict(mlout,testSet)
   mloutpar <- trainRecoPar(uduu,rnk=75,cls=cls)
   predict(mloutpar,testSet,cls=cls)
}

###############################  misc. ***************************##

# see comment about "fake" records above
# 
#    for each "new" user, add a fake
#    # rating of 1 for item 1; and for each "new" item, add a fake rating
#    # of 1 for user 1
#    tmp <- clusterEvalQ(cls,users <- unique(ratingsIn[,1]))
#    allUsers <- unique(unlist(tmp))
#    clusterExport(cls,c('allUsers'),envir=environment())
#    tmp <- clusterEvalQ(cls,items <- unique(ratingsIn[,2]))
#    allItems <- unique(unlist(tmp))
#    clusterExport(cls,c('allItems'),envir=environment())
#    clusterEvalQ(cls,
#      {
#         for (usr in allUsers) {
#            if (!(usr %in% ratingsIn[,1])) 
#               ratingsIn <<- rbind(ratingsIn,c(usr,1,1))
#         };
#         for (itm in allItems) {
#            if (!(itm %in% ratingsIn[,2])) 
#               ratingsIn <<- rbind(ratingsIn,c(1,itm,1))
#         }
#      })
