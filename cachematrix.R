## This first function makeCacheMatrix cache the value of the inverse of a 
## matrix so that when we need it again, it can be looked up in the cache 
## rather than recomputed. The set, get, setinv, getinv can read or change 
## the matrix or the inverse.
## NB : the `<<-` operator assign a value to an object in another environment 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(newinv) inv <<- newinv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(makeResult, ...) {
        minv <- makeResult$getinv()
        mat <- makeResult$get()
        
        if(!is.null(minv)) {
                message("getting cached matrix inverse")
                return(minv)
        }
        minv <- solve(mat)
        makeResult$setinv(minv)
        minv
}


