
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

# House Voting data: change question marks to NAs

getHouseVoting <- function() 
{
   hv <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data',header=F)
   f <- function(cl) gsub('\\?',NA,cl)
   hv <- sapply(hv,f)
   as.data.frame(hv)
}

# get the instructor evaluation data, and set it up, including
# covariates

getInstEval <- function()  {
   data(InstEval)
   ivl <- InstEval
   # make correct format, choose 
   ivl <- ivl[,c(1,2,7,3:6)]
   # convert from factor
   ivl$y <- as.numeric(ivl$y)
   ivl$studage <- as.numeric(ivl$studage)
   ivl$lectage <- as.numeric(ivl$lectage)
   ivl
}

# this builds the iextended ML100K dataset, including the raw data but
# then adding in the covariates

# we need files such as u.data in the directory 'datadir'; either they
# are already there, or if not needDownload = TRUE, the dat is
# downloaded or unzipped to the 'ml-100k' in the current directory

# return value is a data frame with the following columns:

# > names(w)
#  [1] "user"      "item"      "rating"    "timestamp" "age"       "gender"   
#  [7] "occ"       "zip"       "userMean"  "Nuser"     "G1"        "G2"       
# [13] "G3"        "G4"        "G5"        "G6"        "G7"        "G8"       
# [19] "G9"        "G10"       "G11"       "G12"       "G13"       "G14"      
# [25] "G15"       "G16"       "G17"       "G18"       "G19"       "itemMean" 
# [31] "Nitem"

# here G1, G2 etc. are the genres

getML100K <- function(needDownload=FALSE)  
{
   if (needDownload) {
      # 5 Mb
      download.file(
         'http://files.grouplens.org/datasets/movielens/ml-100k.zip',
         'ml-100k.zip')
      unzip('ml-100k.zip')
   }
   currdir <- getwd()  # leave a trail of bread crumbs
   datadir <- 'ml-100k'  # easier to hard code
   setwd(datadir)
   on.exit(setwd(currdir))

   # make matrices ud, uu and ui, for the main ratings data, user info
   # an item info

   ud <- read.table('u.data',header=F,sep='\t')  
   colnames(ud) <- c('user','item','rating','timestamp')
   ud <- as.data.frame(ud)

   uu <- read.table('u.user',header=F,sep='|',stringsAsFactors=TRUE)  
   ur <- split(ud[,3],ud[,1])  # ratings by user
   uu <- cbind(uu,sapply(ur,mean))
   uu <- cbind(uu,sapply(ur,length))
   colnames(uu) <- c('user','age','gender','occ','zip','userMean','Nuser')

   # reading u.item is tricky, with some problematic records etc.;
   # fortunately, we only need the last 19 fields
   z <- readLines('u.item')
   zs <- strsplit(z,'|',fixed=TRUE)  # splits to single characters
   zgl <- lapply(zs,function(charvec) charvec[6:24])  # get the genre dummies
   zgls <- t(sapply(zgl,as.integer))  # create matrix of genres
   ui <- cbind(1:nrow(zgls),zgls)
   ir <- split(ud[,3],ud[,2])  # ratings by item
   ui <- cbind(ui,sapply(ir,mean))
   ui <- cbind(ui,sapply(ir,length))
   colnames(ui) <- c('item',paste('G',1:19,sep=''),'itemMean','Nitem')

   setwd(currdir) # follow the trail back 
   uduu <- merge(ud,uu)
   uduuui <- merge(uduu,ui)
   # this ends up in (item,user) order, whereas we need the opposite
   outdf <- uduuui[,c(2,1,3:ncol(uduuui))]
   attr(outdf,'useritemCovs') <- c(4,4)
   attr(outdf,'userCovs') <- c(5,10)
   attr(outdf,'itemCovs') <- c(11,31)
   outdf
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

### WORK IN PROGRESS

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


# goal of this file:  determine the estimated probability that this user
# will rate this item

# arguments:

#    bigUIRandCovs:  e.g. output of getML100K(); format is
#       (userID; itemID; rating; useritem covs; user covs; item covs)`

#    value: data frame of the same format, but with rows added for the
#       (user,item) combinations not present in the data; rating column
#       is now 1 for rating present, 0 for not; useritem covs removed

getFullUIRandCovs <- function(bigUIRandCovs) 
{
###    # as usual in this pkg, user, item IDs assumed consecutive from 1
###    users <- 1:max(bigUIRandCovs$user)  
###    items <- 1:max(bigUIRandCovs$item)  
###    itemsByUser <- split(bigUIRandCovs$items,users)
###    userCovs <- bigUIRandCovs[,attr(bigUIRandCovs,'userCovs')][1,]
###    itemCovs <- bigUIRandCovs[,attr(bigUIRandCovs,'itemCovs')][1,]
### 
###    doOneUser <- function(oneUser)  # one elt of itemsByUser
###    {
###       missingItems <- setdiff(items,itemsByUser)
###       usercovs <- userCovs[oneuser[1,]
###       # confusing to do *apply() within *apply()
###       for (missingitem in missingItems) {
###       }
###    }
    users <- 1:max(bigUIRandCovs$user)
    items <- 1:max(bigUIRandCovs$item)
    itemsByUser <- split(bigUIRandCovs$items, users)
    userCovs <- bigUIRandCovs[, attr(bigUIRandCovs, "userCovs")][1, 
        ]
    itemCovs <- bigUIRandCovs[, attr(bigUIRandCovs, "itemCovs")][1, 
        ]
    doOneUser <- function(oneUser) {
        missingItems <- setdiff(items, itemsByUser)
        usercovs <- userCovs[oneuser, ]
        for (missingitem in missingItems) {
        }
    }

}

probWillRate <- function(bigUIRandCovs,user,item) 
{

}
getMovieLensGenres <- function() 
{
   movs <- read.table('u.item',sep='|')
   movs[,c(1,6:24)]
}

getMovieLensUserPrefs <- function() 
{

}

########################  asMatrix(input()  ##############################

# userData is output of formUserData()

asMatrix <- function(userData) 
{
   nr <- max(as.numeric(names(userData)))
   nc <- max(sapply(userData,function(elt) max(elt$itms)))
   res <- matrix(nrow=nr,ncol=nc,NA)
   for (i in 1:nr) {
      rw <- userData[[i]]
      res[i,rw$itms] <- rw$ratings
   }
   res
}
