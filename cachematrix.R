##  `CacheMatrix` -- Matrix with cached inverse
##
##  This file provides functions to have a matrix that can cache its own
##  inverse. This allows for speeding up calculations involving the inverse of
##  the same matrix multiple times.
##
##  **Note**, however, that the speed-up comes at a price: the cached inverse
##  will take up the same amount of memory as your original matrix, which
##  effectively doubles the memory requirement for your matrices!
##
##  Example:
##
##      > # Assuming the current working directory contains this file
##      > source("./cachematrix.R")
##      >
##      > # Create a 1000x1000 random matrix and turn it into a `CacheMatrix`
##      > mtx <- matrix(rnorm(1000000), nrow = 1000, ncol = 1000)
##      > cm <- makeCacheMatrix(mtx)
##      >
##      > # The first inverse calculation is "slow"
##      > system.time(cacheSolve(cm))
##         user  system elapsed
##        0.648   0.028   0.403
##      >
##      > # The subsequent inverse calculations are fast...
##      > # ...because they don't happen
##      > system.time(cacheSolve(cm))
##         user  system elapsed
##            0       0       0
##      > system.time(cacheSolve(cm))
##         user  system elapsed
##            0       0       0


##  `cmtx <- makeCacheMatrix(x)`
##
##  Creates a new `CacheMatrix` from the matrix `x`. The cache for the inverse
##  of `x` is empty in the returned `CacheMatrix`, i.e. the first time the
##  inverse is needed, it will have to be computed (see `cacheSolve`).
##
##  Returns a list of four functions that manipulate the matrix and its
##  inverse:
##  -   `cmtx$set(y)`               - set matrix to `y`, clear cached inverse
##  -   `cmtx$get()`                - get matrix
##  -   `cmtx$setInverse(inverse)`  - save `inverse` in cache
##  -   `cmtx$getInverse()`         - get cached inverse
##
##  See also:
##  -   `cacheSolve`
##  -   `?matrix`
makeCacheMatrix <- function(x = matrix()) {
    #   This is the cache variable; NULL = empty cache
    inv <- NULL

    #   Matrix manipulation
    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }
    get <- function()
        x

    #   Cache manipulation
    setInverse <- function(inverse)
        inv <<- inverse
    getInverse <- function()
        inv

    #   Assemble the four functions into the return value
    list(set        = set,
         get        = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##  `inv <- cacheSolve(x, ...)`
##
##  Calculates the inverse of the `CacheMatrix` `x`.
##
##  Returns the inverse of `x`, as computed by the `solve()` function.
##  If the value of the inverse is already cached, it returns the cached value
##  without calling `solve()`.
##  If there is no cached value yet, it calls `solve()` and saves the result in
##  the cache.
##
##  **WARNING**
##      `x` is assumed to be invertible, but no checks are made the verify it!
##      If you pass in a non-invertible matrix, you'll get an error from
##      `solve()`. For example:
##          > mtx <- matrix(rnorm(6), nrow = 2, ncol = 3)
##          > cm <- makeCacheMatrix(mtx)
##          > cacheSolve(cm)
##          Error in solve.default(mtx, ...) : 'a' (2 x 3) must be square
##
##  See also:
##  -   `makeCacheMatrix`
##  -   `?solve`
cacheSolve <- function(x, ...) {
    #   Get the cached value, which can be NULL if the cache is empty
    inv <- x$getInverse()

    #   If we have no cached value yet, compute the result and save it in the
    #   cache for future use
    if (is.null(inv)) {
        mtx <- x$get()
        inv <- solve(mtx, ...)
        x$setInverse(inv)
    }

    #   At this point `inv` is the inverse, either taken from the cache or
    #   holding the result of `solve()`
    inv
}
