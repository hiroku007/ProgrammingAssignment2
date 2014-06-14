## These functions are essentially used to store important values to a list. The first function creates the list,
## and the second function is responsible for calling the special values assigned to the list, or assigning the special
## value to the list if it is not already known.

## This function is responsible for creating the list. This function is composed of 4 sub functions that can be used
## to determine the important values of the list. One function is used to set the initial value, and another value
## is used to obtain the initial value. Another subfunction in this function sets the special value, and the other
## returns the special value.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setInverse<-function(solve) m <<- solve
  getInverse<-function() m
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function is responsible for either returning the special data stored in the list that is defined in the function
## above, or is responsible for calculating the special value, if it is not already assigned. The second part of this
## function also assigns the special value to the list, so that when it is run again, instead of redoing the calculation
## it simply just returns the data stored in the list.

cacheSolve <- function(x, ...) {
  m<-x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    }
  data<-x$get()
  m<-solve(data)
  x$setInverse(m)
  m
}
