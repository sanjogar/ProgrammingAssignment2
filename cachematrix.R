### Assignment 2. Caching the inverse of a matrix

## Put comments here that give an overall description of what your
## functions do



## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse.
# Arguments:
# directory = "specdata"  # Chara

makeCacheMatrix <- function(m = matrix()) {
    #browser()
    inver <- NULL
    set <- function(ww) {
        m <<- ww
        inver <<- NULL
        
        print(m)
        print(ww)
    }
    
    print(m)
    get <- function() m
    
    setinver <- function(solve) inver <<- solve # function that returns the inverse of an invertible matrix
    
    getinver <- function() inver

    list(set = set, get = get, setinver = setinver, getinver = getinver)

}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

debug(cacheSolve)
cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        print(m)
        x <- makeCacheMatrix(m)
        
        browser()
        
        
        
        inver <- x$getinver()
    
        print(inver)
        if(!is.null(inver)) {
            message("Getting cached inverted matrix")
            print("jooo")
            return(inver)
        }
        x$set(m)
        m
        dat <- x$get()
        print(dat)
        inver <- solve(dat, ...)
        x$setinver(inver)
        inver
        
}

