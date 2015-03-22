## These functions when used in conjuction with each other allow you to use the inverse
## of a matrix if it has already been calculated.  If it hasn't been calculated in calculates
## and stores the inverse if it is calculating it so it can be used in the future. 
## These functions assume that the matrix that is inputed is always invertible.

## This function takes a matrix as the input and creates 4 functions that allow 
## for the caching and retriving of the inversese of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                     ##Sets the initial value of inv to NULL        
        set <- function(y) {                            ##The Set function
                x <<- y                                 ##stores the input matrix in the 
                                                        ##makeCacheMatrix environment
                inv <<- NULL                            ##sets inv to NULL in the makeCacheMatrix Environment
        }
        get <- function() x                              
        setinverse <- function(solve) inv <<- solve     ##Function which  
        getinverse <- function() inv
        getevn<- function() environment()
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse,
             getevn = getevn)
}


## Write a short comment describing this functio

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
                }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        }
