## Calculates the inverse of a given matrix, but if the matrix has been 
## already given it skip the calculation and gives the inverse from a 
## list where are stored all the matrix and their inverse that have been
## previously given.

## This function creates a list where the matrix and its inverse are storaged.

makeCacheMatrix<- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes as argument the result from the first function. 
## Takes the inverse from the cached information.

cacheSolve<- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
