## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        IM <- NULL
        set <- function(y) {
                x <<- y
                IM <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(InvMat) IM <<- InvMat
        getInverseMatrix <- function() IM
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        IM <- x$getInverseMatrix()
        if(!is.null(IM)) {
                message("getting cached data")
                return(IM)
        }
        data <- x$get()
        IM <- solve(data, ...)
        x$setInverseMatrix(IM)
        IM
        
}
