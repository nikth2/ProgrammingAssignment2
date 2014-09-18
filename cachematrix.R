## create two functions that calculate and cache the inverse of a matrix


## make Cache Matrix caches the result of the inversion of the input matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## x an invertible matrix
        
        ## returns the inverted matrix as a list with util functions
        
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


## cacheSolve function computes inverse matrix if not been computed before

cacheSolve <- function(x, ...) { 
        
        ## a cache-able matrix (created with function makeCacheMatrix)
        
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
