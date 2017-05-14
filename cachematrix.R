makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
    get <- function() x
    setI <- function(inverse) inversed <<- inverse
    getI <- function() inversed
    list(set = set, get = get, setI = setI, getI = getI)
}

#the function will inverse the original matrix

cacheSolve <- function(x, ...) {
    inversed <- x$getI()
    # checking whether the matrix is already inverted
    if (is.null(inversed)==FALSE) {
        message("getting cached data")
        return(inversed)
    }
    matrixNew <- x$get()
    inversed <- solve(matrixNew, ...)
    x$setI(inversed)
    inversed
}
