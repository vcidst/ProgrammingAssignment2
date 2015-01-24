## Retrieve inverse of a matrix from cache.
## In case it isn't already in cache, compute and put it there.
## Think of makeCacheMatrix() as a public class.

## Retrieves data or cache for solve()
## Returns a list of functions: set, get, setinverse and getinverse
## set and get will set/get the value of the matrix.
## setinverse and getinverse will set/get the value of inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse
             getinverse = getinverse)
}


## Lookup if the inverse matrix is in the cache. If not, compute.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
