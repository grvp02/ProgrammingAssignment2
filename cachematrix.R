## Caching the inverse of the matrix

## makeCacheMatrix function creates a special matrix object that cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
matrixinv <- NULL
set <- function(y)
{
x<<-y
matrixinv<<-NULL
}
get <-function() x
setinverse <- function(solve) matrixinv<<-solve
getinverse <- function() matrixinv
list(set=set, get=get,
setinverse=setinverse,
getinverse=getinverse)
}

##cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...)
{
matrixinv<- x$getinverse()
if(!is.null(matrixinv))
{
message("getting cache data")
return(matrixinv)        
}
data <- x$get()
matrixinv <-solve(data, ...)
x$setinverse(matrixinv)
matrixinv
}
