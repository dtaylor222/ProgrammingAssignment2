## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## in reality this retuns a list of the three functions that are the "methods"
## of the matrix object object, get() , set() and getinverse()
makeCacheMatrix <- function(x = matrix()) {
        
        solution <- NULL # 'solution' is the matrix' inverse

        get <- function () x # this simply returns the matrix contents       
                
        set <- function(y=matrix()){
                x <<- y                 # this method sets the matrix contents directly 
                solution <<- NULL       # and therefore clears the cached solution    
        }
        
        # sets the inverse of the matrix
        setinverse <- function(solve) solution <<- solve
        
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
        result <- x$getinverse()
        if(!is.null(result)) {
                message("getting cached data")
                return(result)
        }
        data <- x$get()
        solution <- solve(data, ...)
        x$setinverse(solution)
        solution
}
