## Put comments here that give an overall description of what your
## functions do
## Caching the Inverse of a Matrix
## For easy access of the inverse of a matrix to overcome the costly computation each and every time 


## Write a short comment describing this function
##Function used for creating the matrix object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) mat <<- inverse
        getInverse <- function() mat
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## Use of the function :-
## Computing the inverse of the above created matrix (If the inverse exists then retreive the inverse from cache)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getInverse()
        if (!is.null(mat)) {
                return(mat)
        }
        m <- x$get()
        mat <- solve(m , ...)
        x$setInverse(mat)
        mat
}
