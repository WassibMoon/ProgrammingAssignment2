## This program writes a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        MInv <- NULL
        set <- function(y){
                x <<- y
                MInv <<- NULL
        }
        get <- function() x
        setInv <- function(inv){
                MInv <<- inv
        }
        getInv <- function() MInv
        list(set = set, get = get, 
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MInv <- x$getInv()
        if(!is.null(MInv)) {
                message("getting cached data")
                return(MInv)
        }
        data <- x$get()
        MInv <- solve(data)
        x$setInv(MInv)
        MInv
}
