# The two following functions are designed to avoid multiple calculation of 
# the inverse of a matrix. It is useful in the sense that one can save compu-
# tational resources by avoiding more than one heavy calculation that is 
# called many times throughout a work in an R program.
# 
# The function makeCacheMatrix() is responsible for saving an evaluated inver-
# se made by cacheSolve(), and for returning a saved inverse after queried by
# cacheSolve().

# THIS IS A GITHUB TEST

makeCacheMatrix <- function(x = matrix()) {
        ii <- NULL
        set <- function(y) {
                x <<- y
                ii <<- NULL
        }
        get <- function() x
        setinv <- function(inv) ii <<- inv
        getinv <- function() ii
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# cacheSolve() first assesses if the inverse of that matrix, saved by means of 
# makeCacheMatrix(), has already been solved. If yes, cacheSolve() returns the
# previously calculated inverse. Otherwise, cacheSolve() will solve the inverse
# and call the functions inside makeCacheMatrix() to get and save the solved
# inverse in a parent environment, through the use of the deep assignment <<-.

cacheSolve <- function(x, ...) {
        ii <- x$getinv()
        if(!is.null(ii)) {
                message("getting cached data")
                return(ii)
        }
        data <- x$get()
        ii <- solve(data, ...)
        x$setinv(ii)
        ii
}

# To test the functions, you can use debug(cacheSolve) to understand the 
# step-by-step of the function. You will see that once 
# the inverse has been previously evaluated, when you call cacheSolve() again 
# for the same "matrix" (created with makeCacheMatrix()) the function will 
# simply display the value of the inverse, stored as ii in the namespace
# of the function cacheSolve. Try this to run the functions:

debug(cacheSolve)
x1 <- matrix(2:5, 2, 2)
x1
x2 <- makeCacheMatrix(x1)
x3 <- cacheSolve(x2)
x3
# In the following line the function returns the stored value, without solving
# it again.
x4 <- cacheSolve(x2) 
x4


