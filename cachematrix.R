## This is the 2nd assignment done by mkdaniel1
## These functions will create a matrix and then caluculate the inverse of that matrix 

## This function creates an invertable matrix and caches it

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y){
                x <<- y
                im <<- NULL
        }
        get  <- function() x
        setinv <- function(inv) im <<- inv
        getinv <- function() im
       list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function calculates the inverse of the matrix and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinv()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data,...)
        x$setinv(im)
        im
}
