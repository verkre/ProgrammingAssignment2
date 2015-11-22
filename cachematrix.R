## the function makeCacheMatrix creates an object with two private variables (myMatrix, invertedMatrix)
## and allows working with them by calling one of four functions (set, get, setInverse, getInverse) 
## defined within the object constructor makeCacheMatrix 

makeCacheMatrix <- function(myMatrix = matrix()) {
        invertedMatrix <- NULL
        set <- function(newMatrix) {
                myMatrix <<- newMatrix
                invertedMatrix <<- NULL
        }
        get <- function() myMatrix
        setInverse <- function(newInvertedMatrix) invertedMatrix <<- newInvertedMatrix
        getInverse <- function() invertedMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## the function cacheSolve checks whether the inverse of a given CacheMatrix (x) was
## already computed. If so, it returns the inverse. If not, the inverse is computed,
## saved in x, and returned.

cacheSolve <- function(x, ...) {
        invertedMatrix <- x$getInverse()
        if(!is.null(invertedMatrix)) {
                message("getting cached data")
                return(invertedMatrix)
        }
        data <- x$get()
        invertedMatrix <- solve(data, ...)
        x$setInverse(invertedMatrix)
        invertedMatrix
}
