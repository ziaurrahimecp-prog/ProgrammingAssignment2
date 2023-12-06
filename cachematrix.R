## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function will create a matrix function that allows user to set their matrix ($set), read the matrix($get)
## This function will also allow user to read the inverse of function ($getinv)
## If the result is NULL, then user need to run the cacheSolve() function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Write a short comment describing this function

## If makeCacheMatrix$getinv() is NULL, then user need to run the following function
## This function will compute the inverse of matrix and store the result to the function that was 
## previously created in makeCachematric()


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
