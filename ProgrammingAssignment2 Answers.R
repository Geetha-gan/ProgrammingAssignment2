# Programming Assignment2 makeCacheMatrix

makeCacheMatrix <- function(m = matrix()) { # creating a matrix that can catch its inverse
    i <- NULL  # initializing NULL to inverse
    set <- function(matrix) {                   # sets a vector x to a new vector y and resets the mean
        m <<- matrix
        i <<- NULL
    }
    get <- function() m                    # returns the matirx m
    setInverse <- function(inverse) 
        i <<- inverse                      # sets the inverse to i
    getInverse <- function() 
        i                                  # returns the inverse of the matrix
    list(set = set, get = get,             
                                           # returns the vector with all the functions just defined
         setInverse = setInverse,
         getInverse = getInverse)
}



# Programming Assignment2 cacheSolve

cacheSolve <- function(x, ...) {    # to return a matrix that is inverse of x
    m <- x$getInverse()
    if(!is.null(m)) {              # return inverse if already set
        message("getting cached data")
        return(m)
    }
    data <- x$get()                # get the matrix from our data
    m <- solve(data) %*% data      # calculation of inverse
    x$setInverse(m)                # setting the inverse
    m                              # return the matrix
}