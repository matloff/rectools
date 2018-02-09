
# utility to read in raw data in standard format,

#    (user ID, item ID, rating)

# and form an R list for user data, with class 'usrData'; each element
# of the list will be of class 'usrDatum', representing one user's data,
# and will have components as seen in 'value' below 

# arguments:

#    ratingsIn: input data, whose first 3 cols are user ID, item ID
#               and rating 
#    usrCovs: data frame of user covariates, e.g. gender and age, one
#             row per user; optional
#    itmCats: data frame of item categories, e.g. genre for movies, one
#             row of booleans per item; categories need not be 
#             mutually exclusive; optional
#    fileOut: if specified, save the value returned by the function
#             using R save(), with file name fileOut

# value:

#    object of class 'usrData': an R list with one element per user;
#    each such element is itself an R list, an object of class
#    'usrDatum', with these components:
#
#       userID:  the ID of this user
#       ratings:  vector of ratings made by this user
#       itms:  IDs of items rated by this user
#       cvrs:  covariate data for this user, if any
#       cats:  item category data for this user, if any; j-th element
#              is proportion of items rated by this user that are 
#              in category j; e.g. for this user, 0.22 of the movies he
#              rates are comedies, 0.07 are historical dramas, etc.; 
#              again, sum could be greater or less than 1.00

formUserData <- function(ratingsIn,usrCovs=NULL,itmCats=NULL,fileOut='') {

   # covariates, if any, should be in usrCovs, not in ratingsIn; check
   # to see if user has included them
   if (ncol(ratingsIn) > 3)
      stop('ratingsIn more than 3 columns')

   # IMPORTANT NOTE: in order to work in cross-validation, etc. we need
   # to abandon the idea of having the user IDs start at 1 and be
   # consecutive; instead, we will just use the ID numbers as list
   # indices; e.g. if we have users numbered 2,8,85 then retval below
   # will consist of retval[[2]], retval[[8]] and retval[[85]]

   # rownums[[i]] will be the row numbers in ratingsIn belonging to user i
   rownums <- split(1:nrow(ratingsIn),ratingsIn[,1])
   nusers <- length(rownums)
   nitems <- length(unique(ratingsIn[,2]))

   if (!is.null(itmCats)) {
      # must convert from a data frame row to a numeric vector
      itmCats <- as.matrix(itmCats)
      nitms <- nrow(itmCats)
      if (nitms != nitems) stop('itmCats has too many/few rows')
   }

   # retval will ultimately be the return value, a list of lists as
   # described above.
   retval <- list()

   for (i in 1:nusers) {
      whichrows <- rownums[[i]]  # row nums in ratingsIn for user i
      userID <- as.character(ratingsIn[whichrows[1],1])
      # start building usrDatum object for this user
      retval[[userID]] <- list()
      retval[[userID]]$userID <- userID
      retval[[userID]]$itms <- ratingsIn[whichrows,2]
      retval[[userID]]$ratings <- ratingsIn[whichrows,3]
      names(retval[[userID]]$ratings) <- as.character(retval[[userID]]$itms) 

      if (!is.null(usrCovs))
         # change from data frame row to numeric vector
         retval[[userID]]$cvrs <- as.numeric(usrCovs[userID,])
      if (!is.null(itmCats)) {
         # form tmp, a boolean vector indicating which items were or
         # were not rated by this user
         tmp <- rep(0,nitems)
         tmp[retval[[userID]]$itms] <- 1
         # form vector whose j-th element will be the count of items
         # rated by this user that are in category j
         catcounts <- tmp %*% itmCats
         # convert to proportions and record
         retval[[userID]]$cats <- catcounts / sum(tmp)
      }
      class(retval[[userID]]) <- 'usrDatum'
   }
   class(retval) <- 'usrData'
   if (fileOut != '') save(retval,file=fileOut)
   retval
}

## # construct a new object of class 'usrDatum'
## formUserDatum <- function(itms,ratings,userID=NULL) {
##     obj <- list(itms = itms, ratings=ratings,userID=userID)
##     class(obj) <- 'usrDatum'
##     obj
## }

# utility:  find input row for a given user, item
findInputRow <- function(ratingsIn,usrID,itmID) {
   ratingsIn[ratingsIn[,1]==usrID & ratingsIn[,2]==itmID,]
}

testFormUserData <- function() 
{
   rts <- rbind(c(1,3,5),c(4,2,2),c(4,1,2),c(5,6,2),c(1,6,5))
   formUserData(rts) 

}
