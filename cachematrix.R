## The following two functions return the inverse of a matrix using the solve() function
## The cacheSolve should be run to generate the solution
## The function returns the cached value of the inverse if the argument to cacheSolve is NULL to save time, 
## otherwise the inverse is calculated based on the input matrix
## Currently there is no error checking to insure the matrix is invertible


makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix creates two functions to set and get a matrix from cached memory
## the argument is a matrix variable
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


cacheSolve <- function(x, ...) {
        ## cacheSolve returns a matrix that is the inverse of 'x' where 'x' is the matrix in cached memory
		## Display results after solving for inverse
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
