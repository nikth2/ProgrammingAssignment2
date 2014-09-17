## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        
        list(set=set, get=get, set_inverse = setInverse, get_inverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$get_inverse()
        
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx)
        x$set_inverse(inv)
        inv
        
}
