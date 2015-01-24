## This is a pair of functions that cache the inverse of a matrix.
## They will save computing time so that we don't have the same 
## matrix's inverse twice. This advantage becomes more obvious if we are
## working with a large matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ## retrieve 'x'
        setinverse <- function(inverse) m <<- inverse
        ## store the inverse in the parent environment
        getinverse <- function() m
        ## retrieve m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        ## create a list here so that in cacheSolve, we can use the 
        ## $ sign to call up any of the four function
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse() 
        ## retrieve m
        if(!is.null(m)) {   
                message("getting cached data")
                return(m)
                ## if the inverse is computed before (and thus would be found
                ##in the parent environment), get cached data
        }
        data <- x$get()m 
        ## assign input 'x' in 'data'
        m <- solve(data, ...) 
        ## if the inverse has not been computed, compute it here
        x$setinverse(m) 
        ## and store the inverse in m, in the parent environment
        m
        ## Return a matrix that is the inverse of 'x'
}
