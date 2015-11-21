## In general the following functions supports to make the costly
## matrix inverse calculation cachable.

makeCacheMatrix <- function(x = matrix()) {
    ## Returns a wrapper which can store a matrix and cache the inverse of it
    
    ## I use self. notation to point these are some kind of object fields
    self.inverse <- NULL

    set <- function(aMatrix) {
        ## Store the matrix and invalidate the cache
        self.matrix <<- aMatrix
        self.inverse <<- NULL
    }
    get <- function() {
        self.matrix
    }
    setinverse <- function(inverse) {
        ## Store the cache on the object
        self.inverse <<- inverse
    }
    getinverse <- function() {
        self.inverse
    }
    
    ## Gives back the object(ish) list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(wrapper, ...) {
    ## Returns a matrix that is the inverse of 'wrapper'
    ## Side effect of this function is that it's store the calculated
    ## value on the given matrix wrapper.
    inverse <- wrapper$getinverse()
    if(!is.null(inverse)) {
        ## If the inverse has calculated gives it back
        return(inverse)
    }
    aMatrix <- wrapper$get()
    
    ## caluclate the inverse and store on the wrapper
    wrapper$setinverse(solve(aMatrix, ...))
    
    ## gives back the inverse
    wrapper$getinverse()
}