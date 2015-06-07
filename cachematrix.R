## Programming in R - Assignment 2

## Returns a list of function calls that control the state of a matrix
## x and its inverse (allowing us to cache the task of computing the 
## inverse matrix)
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    # Assigns matrix x and clears inverse
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    # Returns x
    get <- function(){ 
        x    
    }
    
    # Sets inverse matrix
    setInverse <- function(inv){
        inverse <<- inv
    }
    
    # Returns inverse matrix
    getInverse <- function(){
        inverse
    }
    
    # Return list
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Solves and returns the inverse of a matrix x. We use the above list
## to store and access the value at any time without recomputing the result
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        
        # If an inverse is stored already
        if(!is.null(inverse)){
            message("Fetching cached data")
            return(inverse)
        }
        
        mat <- x$get()
        inverse <- solve(mat)
        x$setInverse(inverse)
        
        inverse
}
