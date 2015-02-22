## Put comments here that give an overall description of what your
## functions do

## This function creates a list containing a function to:
## set the value of a matrix
## get the value of a matrix
## set the value of the inverse of a matrix
## get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the value of the inverse of a matrix. 
## It checks first if the inverse has already been computed. 
## If yes, it says "getting cached data".
## If no, it compute the value of the inverse and set it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
      }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  return(inv)
}

## > x=rbind(c(1, 3), c(3,1))
## > m=makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    3    1

## cacheSolve(m)
## [,1]   [,2]
## [1,] -0.125  0.375
## [2,]  0.375 -0.125

## > cacheSolve(m)
## getting cached data
## [,1]   [,2]
## [1,] -0.125  0.375
## [2,]  0.375 -0.125