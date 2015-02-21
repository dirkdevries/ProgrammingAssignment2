## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
#     message("MCM 8")
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
# message("MCM 13")
    get <- function() x
     setmatrix <- function(m) m <<- matrix
     getmatrix <- function() m 
     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getmatrix()
     #message("Print m in cacheSolve:")
     # check the value of m.
     # if m != null: data is in cache available
     # so m is returned from the cache
          if(!is.null(m)) {
         message("getting cached data")
         return(m)
     }
     # if m == null
     data <- x$get()
     m <- solve(data, ...)
     x$setmatrix(m)
     #message("cS if m == nul m:")
     m
}
cacheSolve(makeCacheMatrix(2*diag(3)))
d <- replicate(2, rpois(2, 5))  
d
mm <- makeCacheMatrix(d)
cacheSolve(makeCacheMatrix(d))
mm$get()
mm$getmatrix()
d <- cbind(c(5,10), c(6,9))

