## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates  matrix object 
makeCacheMatrix <- function( mat = matrix() ) {
    
    ## Initialize  property
    inv <- NULL
    
    ## Method to set 
    set <- function( matrix ) {
        mat <<- matrix
        inv <<- NULL
    }
    
    ## Method the get 
    get <- function() {
        ## Return
        mat
    }
    
    ## Method to set 
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    ## Method to get 
    getInverse <- function() {
        ## Return the inverse
        inv
    }
    
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
    
    m <- x$getInverse()
    
    
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the matrix 
    data <- x$get()
    
    ## Calculate the inverse 
    m <- solve(data) %*% data
    
    ## Set the inverse 
    x$setInverse(m)
    
    ## Return 
    m
}
