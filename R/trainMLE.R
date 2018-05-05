 
# arguments:

#   ratingsIn: input data, with cols (userID,itemID,rating,
#              covariates); data frame
#   binaryCase: "Y" has just 2 values; glmer() will be used, not lmer()
#   cls: an R 'parallel' cluster

# in the parallel case, use 'partools' philosophy of Leave It There;
# run lmer() on each chunk, leaving the output lmerout there; then when
# predicting, we call predict(lmerout,testset) at each node, then
# average the results (with na.rm = TRUE)

# value:

#   ydotsMLE (if NULL cls): object of class 'lmer' (lmer4 pkg)

#   ydotsMLEpar (if non-NULL cls): S3 class with components of class ydotsMLE

trainMLE <- function(ratingsIn,binaryCase=FALSE,cls=NULL,printTimes=TRUE) {
  require(lme4)
  nms <- names(ratingsIn)
  haveCovs = ncol(ratingsIn) > 3
  if (!haveCovs) {
     tmp = sprintf('%s ~ (1|%s) + (1|%s)',
        nms[3],nms[1],nms[2])
     frml = as.formula(paste(tmp))
  } else {
     frml <- paste(nms[3],'~ ')
     for (i in 4:ncol(ratingsIn)) {
        frml <- paste(frml,nms[i])
        frml <- paste(frml,'+')
     }
     frml <- paste(frml,'(1|',nms[1],')',
                      '+ (1|',nms[2],')')
     frml <- as.formula(frml)
  }
  if (is.null(cls)) {
     lmerout <- 
        if (!binaryCase) lmer(frml,data=ratingsIn) else
        glmer(frml,data=ratingsIn,family=binomial) 
  } else {
     if (binaryCase) 
        stop('parallel version does not accommodate binary case yet')
     require(partools)
     clusterEvalQ(cls,require(lme4))
     tmp <- system.time(
        distribsplit(cls,'ratingsIn')
     )
     if (printTimes) cat('distribsplit time: ',tmp,'\n')
     clusterExport(cls,'frml',envir=environment())
     clusterExport(cls,c('nms','haveCovs'),envir=environment())
     lmerout <- clusterEvalQ(cls,lmerout <- lmer(frml,data=ratingsIn))
     ### ydots = clusterEvalQ(cls,formYdots(ratingsIn,nms,haveCovs,lmerout))
     lmerout <- list()  # nothing to return
     class(lmerout) = 'ydotsMLEpar'
  }
  attr(lmerout,'binaryCase') <- binaryCase
  invisible(lmerout)
}

# predict() method for the 'ydotsMLE' class
#
# testSet in same form as ratingsIn in train(), except that there 
# is no ratings column
#
# returns vector of predicted values for testSet

predict.ydotsMLE <- function(ydotsObj,testSet,allow.new.levels=TRUE) {
   predict(ydotsObj,testSet,allow.new.levels=allow.new.levels)
}

# predict() method for the 'ydotsMLE' class
predict.ydotsMLEpar <- 
      function(ydotsMLEparObj,testSet,allow.new.levels=FALSE,cls) 
{
   clusterExport(cls,c('testSet','allow.new.levels'),envir=environment())
   preds <- clusterEvalQ(cls,predict(lmerout,testSet,
      allow.new.levels=allow.new.levels))
   Reduce('+',preds)/length(cls)
}

# check
checkydmle <- function() {
   check <- 
      data.frame(userID = c(1,3,2,1,2),itemID = c(1,1,3,2,3),ratings=6:10)
   print(check)
   print(trainMLE(check))
   check$cv <- c(1,4,6,2,10)
   print(check)
   print(trainMLE(check))
}


