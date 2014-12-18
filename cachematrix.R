## R Programming Assignment 2
## 
## given a matrix, return an object which has the matrix along with ability to cache its inverse.
## also define methods as functions- set (store the matrix),
##				 get (get the original matrix),
##				 setInverse (store the Inverse),
##				 getInverse (rerieve the Inverse)


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## Write a short comment describing this function
## Check if inverse matrix of x is cached, if so return cached value
##	otherwise, solve for the inverse, cache it for future, than return Inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
