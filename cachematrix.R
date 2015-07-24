## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## in reality this retuns a list of the four functions that are the "methods" of the object
makeCacheMatrix <- function(x = matrix()) {
        
        solution <- NULL # 'solution' is the cached matrix inverse
        
        setcache <- function(y){
                x <<- y         # this method sets the matrix directly 
                solution <<- NULL      # and therefore overwrites the cache    
        }
        
        get <- function () x # this simply returns the matrix
        
        # gets the inverse of the matrix
        getinverse <- function(){
                solution <- solve(x)
        }
        
        list(set = setcache, get = get, getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
