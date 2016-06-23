
# note: it is assumed that user IDs are consecutive numbers starting at
# 1

# utilities to read in raw data and form an R list for user data,
# with class 'usrData' 

# arguments:

#    ratingsIn: input data, whose first 3 cols are user ID, item ID
#               and rating 
#    usrCovs: data frame of user covariates, e.g. gender and age, one
#             row per user
#    itmCovs: data frame of item categories, e.g. genre for movies,
#             row per item; categories need not be mutually exclusive
#    fileOut: if specified, save the value returned by the function
#             using R save(), with file name fileOut

# value:

#    object of class 'usrData': an R list with one element per user;
#    each such element is itself an R list, with these components:
#
#       ratings: ratings set by this user
#       cvrs:  covariate data for this user, if any
#       itms:  item category data for this user, if any; i-th element
#              is proportion of items rated by this user that are 
#              in category i

formUserData <- function(ratingsIn,usrCovs=NULL,itmCats-NULL,fileOut='') {
   class(retval) <- 'usrData'
   # rownums[[i]] will be the row numbers in ratingsIn belonging to user i
   rownums <- split(1:nrow(ratingsIn),ratingsIn[,1])
   nusers <- length(rownums)
   retval <- list(length=nusers)
   for (i in 1:length(usrs)) {
      retval[[i]]$ratings <- split(ratingsIn[,3],ratingsIn[,1])
      if (!is.null(covCols))
   }

}
