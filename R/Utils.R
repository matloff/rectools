
# utility functions

##############  ratingness(), covratingness() ########################

# finds and returns the number of ratings for each user (splitCol = 1)
# or each item (splitCol = 2), for the input data frame ratingsIn, in
# UserID | ItemID | Rating ...  format

ratingness <- function(ratingsIn,splitCol) {
   tapply(ratingsIn[,3],ratingsIn[,splitCol],length)
}


# forms a column 'nrats', intended to be appended to ratingsIn, with 
# nrats[i] = number of ratings of user i or item i; ratingsIn is as in
# ratingness() above

covratingness <- function(ratingsIn,splitCol) {
   tmp <- ratingness(ratingsIn,splitCol)
   tmp[ratingsIn[,splitCol]]
}

############  prep for some specific example data sets ##############

# get the instructor evaluation data, and set it up, including
# covariates

getInstEval <- function()  {
   data(InstEval)
   ivl <- InstEval
   # make correct format, choose 
   ivl <- ivl[,c(1,2,7,3:6)]
   # create dummy variables in place of dept
   dms <- factorToDummies(ivl$dept,'dpt')
   dms <- as.data.frame(dms)
   dms$dpt2 <- NULL
   ivl$dept <- NULL
   ivl <- cbind(ivl,dms)
   # convert from factors
   ivl$s <- as.numeric(as.character(ivl$s))
   ivl$d <- as.numeric(as.character(ivl$d))
   ivl$y <- as.numeric(ivl$y)
   ivl$service <- as.numeric(ivl$service == 1)
   ivl$studage <- as.numeric(ivl$studage)
   ivl$lectage <- as.numeric(ivl$lectage)
   gassign('ivl','ivl')
}

# get the MovieLens 100K evaluation data, and set it up, including
# covariates

# if needDownload, then download will be done; datadir is the directory
# containing the data, assumed by default to be in the current working
# directory

getML100K <- function(needDownload=FALSE,datadir='./ml-100k')  {
   if (needDownload) {
      # 5 Mb
      download.file(
         'http://files.grouplens.org/datasets/movielens/ml-100k.zip',
         'ml-100k.zip')
      unzip('ml-100k.zip')
   }
   currdir <- getwd()  # leave a trail of bread crumbs
   setwd(datadir)
   ud <- read.table('u.data',header=F,sep='\t')  
   uu <- read.table('u.user',header=F,sep='|')  
   ui <- read.table('u.item',header=F,sep='|')  
   setwd(currdir) # follow the trail back 
   ud <- ud[,-4]   # remove timestamp, leaving user, item, rating  
   uu <- uu[,1:3]  # user, age, gender  
   ui <- ui[,c(1,6:24)]  # item num, genres  
   names(ud) <- c('user','item','rating')  
   names(uu) <- c('user','age','gender')  
   names(ui)[1] <- 'item'  
   names(ui)[-1] <- gsub('V','GNR',names(ui)[-1]) # GNR = genre  
   uu$gender <- as.integer(uu$gender == 'M')  
   uduu <- merge(ud,uu)
   uduuui <- merge(uduu,ui)
   gassign('uduu','uduu')
   gassign('uduuui','uduuui')
}

############################ global assignment #######################

# CRAN bans the use of global variables, but they are sometimes needed;
# one of the CRAN maintainers, Kurt Hornik, encouraged me to use "trick"
# workarounds, so that I would not have to have the CRAN people
# "manually" check my requests for exceptions; gassign() is a vehicle
# for this

# assigns 'rhsname' to the global 'gname', from one level below global

gassign <- function(gname,rhsname) {
   cmd <- paste(gname,'<<-',rhsname)
   eval(parse(text=cmd),envir=parent.frame())
}

################### xval partiitioning routines ########################

# getTrainSet():

# arguments:
#    ratingsIn: the usual raw input matrix, usrID, itmID, rating cols 
#    trainprop: probability that a row from ratingsIn is selected for
#               the training set

# value:
#    training set, in the format of ratingsIn, plus a component
#    trainidxs, the indices of the training set in the original data

getTrainSet <- function(ratingsIn,trainprop = 0.5)
{
   rownew = nrow(ratingsIn)
   trainRow = floor(trainprop*rownew)
   trainidxs = sample(1:rownew,trainRow)
   trainSet = ratingsIn[trainidxs,]
   trainSet$trainidxs = trainidxs
   trainSet
} 

# getTestSet():

# returns the set-theoretic complement of the training set, to be used
# as the test set

getTestSet <- function(ratingsIn, trainSet)
{
   ratingsIn[-trainSet$trainidxs,]
}

########################  getCovCols()  ##############################

# breaks the covariates column numbers into user covariates and item
# covariates; the former, if any, must precede the latter, if any;
# column ranges are returned as vectors, e.g. 4:6

getCovCols <- function(userCovsStartCol=NULL,itemCovsStartCol=NULL,ncolRatingsIn)
{
   ncolsTot <- ncolRatingsIn
   if(!is.null(userCovsStartCol)) {
      usrCols <- if (is.null(itemCovsStartCol)) 4:ncolsTot else
                 4:(itemCovsStartCol-1)
   } else usrCols <- NULL
   if(!is.null(itemCovsStartCol)) {
      itmCols <- itemCovsStartCol:ncolsTot
   } else itmCols <- NULL
   list(usrCols=usrCols,itmCols=itmCols)
}

########################  factorToDummies()  ##############################

# create a data frame of dummy variables from the factor f; col names
# will optionally be prefixed by prfx

factorToDummies <- function(f,prfx=NULL) {  
   lf <- length(f)
   d <- data.frame(z=rep(0,lf))
   for (l in levels(f)) {
      tmp <- as.integer(f == l)
      nm <- as.character(l)
      if (!is.null(prfx)) nm <- paste(prfx,nm,sep='')
      d[[nm]] <- tmp
   }
   d$z <- NULL
   d
}

########################  getCatPrefs()  ##############################

# inputs a data frame whose first column is user IDs and succeeding
# columns are dummies on item categories, e.g. genres for
# films or music; outputs a data frame

### NEEDS WORK

## Browse[2]> z
##      [,1] [,2] [,3]
##      [1,]    5    0    1
##      [2,]    2    0    1
##      [3,]    5    0    1


getCatPrefs <- function(usrInfo)
{
   # rows of usrIDs by user
   n <- nrow(usrInfo)
   usrRowGrps <- split(1:n,usrInfo[,1])
   # for a given user, find his/her prefs
   oneUsrCatPrefs <- function(usrRows) {
      usrChunk <- usrInfo[usrRows,,drop=FALSE]
      tmp <- colSums(usrChunk[,-1,drop=FALSE])
      tmp / nrow(usrChunk)
   }
   res <- t(sapply(usrRowGrps,oneUsrCatPrefs))
   rownames(res) <- names(usrRowGrps)
   res
}
