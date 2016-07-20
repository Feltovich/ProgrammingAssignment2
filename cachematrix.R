## These functions check to see if the inverse of a matrix has already
## been cached, and the inverse has not been cached, it calculates
## the inverse of the matrix and caches it so that the repetitive and
## potentially costly computation is not repeated unnecessarily

# first function creates a function to cache matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# second function checks to see if inverse has already been calcuated and
# retreives inverse from cache if it has.  If the inverse has not been
# calculated or if the matrix has changes, it recalculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

testmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))

cacheSolve(testmatrix)
