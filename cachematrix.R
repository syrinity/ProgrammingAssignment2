## These functions when used in conjuction with each other allow you to use the inverse
## of a matrix if it has already been calculated.  If it hasn't been calculated in calculates
## and stores the inverse if it is calculating it so it can be used in the future. 
## These functions assume that the matrix that is inputed is always invertible.

## This function takes a matrix as the input and creates 4 functions that allow 
## for the caching and retriving of the inversese of a matrix

makeCacheMatrix <- function(x = matrix()) {
        ##Sets the initial value of inv to NULL        
        inv <- NULL                            
        ##The Set function
        set <- function(y) {                                 
                ##stores the input matrix in the makeCacheMatrix environment
                x <<- y                            
                ##sets inv to NULL in the makeCacheMatrix Environment
                inv <<- NULL
        }
        get <- function() x     
        ##Function which will calculate the inverse of the matrix                              
        setinverse <- function(solve) inv <<- solve                    
        ##Function that will return the cached value for the inverse 
        getinverse <- function() inv                      
        ##returns the Functions 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This funtion uses the matrix from the MakeCacheMatrix function and checks to see if the inverse for
## that matrix has been calculated and stored in which case it returns the cached value.  If it hasn't
## it calculates the inverse and caches it for future use
## It will return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ##gets the value of the inverse of the matrix from the environment "x"
        inv <- x$getinverse()
        ##Checks to see if the value is null and if it isn't it returns the value
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
                }
        ##pulls the matrix from the makeCacheMatrix into "data"
        data <- x$get()
        ##uses solve to calculate the inverse of the matrix
        inv <- solve(data, ...)
        ##caches the inverse of the matrix
        x$setinverse(inv)
        ##returns the inverse of the matrix
        inv
        }
