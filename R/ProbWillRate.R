
# goal of this file:  determine the estimated probability that this user
# will rate this item

# UNDER CONSTRUCTION

# arguments:

#    bigUIRandCovs:  e.g. output of getML100K(); format is
#       (userID; itemID; rating; useritem covs; user covs; item covs)`

#    value: data frame of the same format, but with rows added for the
#       (user,item) combinations not present in the data; rating column
#       is now 1 for rating present, 0 for not; useritem covs removed

## getFullUIRandCovs <- function(bigUIRandCovs) 
## {
##    # as usual in this pkg, user, item IDs assumed consecutive from 1
##    users <- 1:max(bigUIRandCovs$user)  
##    items <- 1:max(bigUIRandCovs$item)  
##    itemsByUser <- split(bigUIRandCovs$items,users)
##    userCovs <- bigUIRandCovs[,attr(bigUIRandCovs,'userCovs')][1,]
##    itemCovs <- bigUIRandCovs[,attr(bigUIRandCovs,'itemCovs')][1,]
## 
##    doOneUser <- function(oneUser)  # one elt of itemsByUser
##    {
##       missingItems <- setdiff(items,itemsByUser)
##       usercovs <- userCovs[oneuser[1,]
##       # confusing to do *apply() within *apply()
##       for (missingitem in missingItems) {
##       }
##    }
## }
## 
## probWillRate <- function(bigUIRandCovs,user,item) 
## {
## 
## }
