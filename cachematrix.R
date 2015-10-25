## Functions that cache the inverse of a matrix-10/25/2015
## The makeCacheMatrix function creates a list that contains functions to
##      1. set the matrix
##      2. get the matrix
##      3. Set the inverse of the matrix
##      4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix" created above. It first checks if the inverse has already been calculated. If so, it gets the inverse from the cache. Otherwise, it calculates the inverse and sets the result of the inverse in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$getmatrix()
        ## Return a matrix that is the inverse of 'x'
        m <- solve(matrix, ...)
        ## set the value of the inverse in the cache
        x$setinverse(m)
        m
        
}

# Test
#my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#cacheSolve(my_matrix)
#cacheSolve(my_matrix)
