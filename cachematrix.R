## makeCacheMatrix sets or gets the matrix to be inverted later with casheSolve
## and returns a list of functions for the matrix and its inverste

makeCacheMatrix <- function(x = matrix()) {
	  Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(I) Inv <<- I
        getInv <- function() Inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}

## cacheSolve solves the inverse of the cached matrix. If the matrix has 
## already been inverted, it will return it otherwise it will calculate it then 
## return it 

cacheSolve <- function(x, ...) {
Inv <- x$getInv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data)
        x$setInv(Inv)
        Inv
        ## Return a matrix that is the inverse of 'x'
}
