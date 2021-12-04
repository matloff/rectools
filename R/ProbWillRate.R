
# goal of this file:  determine the estimated probability that this user
# will rate this item

# arguments:

#    bigUIRandCovs:  e.g. output of getML100K(); format is
#       (userID, itemID, rating, user and item covariates)`

#    value: data frame of the same format, but with rows added for the
#       (user,item) combinations not present in the data; rating column
#       is now 1 for rating present, 0 for not

getFullUIRandCovs <- function(bigUIRandCovs) 
{
   # as usual in this pkg, user ID is assumed consecutive from 1
   users <- 1:max(bigUIRandCovs$user)  
   itemsByUser <- split(bigUIRandCovs$items,users)
}

probWillRate <- function(bigUIRandCovs,user,item) 
{

}
