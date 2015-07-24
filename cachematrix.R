## Programming assignemnt 2 the file contains all that is required to create 
## a matrix using functions makeCacheMatrix() and cacheSolve() that returns
## the inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse.
## in reality this retuns a list of the three functions that are the "methods"
## of the matrix object object, get() , set(), setinverse() and getinverse()
makeCacheMatrix <- function(x = matrix()) {
        
        solution <- NULL # 'solution' is the matrix' inverse

        get <- function () x # this simply returns the matrix contents       
                
        set <- function(y=matrix()){
                x <<- y                 # this method sets the matrix contents directly 
                solution <<- NULL       # and therefore clears the cached solution    
        }
        
        # sets the inverse of the matrix
        setinverse <- function(solve) solution <<- solve # still cant quite believe R !
        
        # simlpy returns the matrix' inverse
        getinverse <- function() solution
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        result <- x$getinverse() # whats currnetly cached ?
        if(!is.null(result)) {   # something was cached so ...
                message("getting cached data") # lets us know that this is the cache
                return(result)
        }
        # nothing is in the cache for us to end here so well ahve to solve and store it
        data <- x$get() # the data matrix is the actual matix in the object
        solution <- solve(data, ...)  # solve funtion returns the inverse of data
        x$setinverse(solution) # This is what actually stores the cache
        solution
}
