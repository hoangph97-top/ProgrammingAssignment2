## This function creates a special matrix object that can cache its inverse Testing testing

makeCacheMatrix <- function(x = matrix()) {
    # 'i' will store the cached inverse. Intialize to NULL
    i <- NULL
    
    # 1. Function to set the matrix value
    set <- function(y) {
        x <<- y #assign the new matrix to 'x' in the parent environment
        i <<- NULL #reset the inverse to NULL, since the matrix has changed
    }
    
    # 2. Function to get the matrix value
    get <- function() x
    
    # 3. Function to set the inverse value
    setInverse <- function(inverse) i <<- inverse
    
    # 4. Function to get the inverve value 
    getInverse <- function() i
    
    #return a list of all four functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been
## calculated (and the matrix has not changed), then it should
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    # get the cached inverse
    i <- x$getInverse()
    
    # 1. Check if the inverse is not NULL (i.e., it is cached)
    if(!is.null(i)) {
        message("getting cached data")
        return(i) # Return the cached inverse
    }
    
    # 2. If it is NOT cached, get the matrix data
    data <- x$get()
    
    # 3. Compute the inverse of the matrix using the 'solve' function 
    i <- solve(data, ...)
    
    # 4. Set the inverse in the cache (in the special matrix object)
    x$setInverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    return(i)
}
