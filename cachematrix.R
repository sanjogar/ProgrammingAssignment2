### Assignment 2. Caching the inverse of a matrix

## Put comments here that give an overall description of what your
## functions do




## -------------------------------------------

# This function creates a new environment into which objects, in this case a specific matrix "m" together with its inverse "inver", can be stored in cache.
# It sets the values to these variables and gives the location of where these two objects are stored in cache, so they can be called from another function trough the list that this function returns.

# Arguments:
# m = specific matrix which value and inverse are stored in cache

makeCacheMatrix <- function(m = matrix()) {

    inver <- NULL
    
    # Creating a new environment for the specific matrix.
    # Setting the value of the matrix to the variable "m" and initialising to NULL the "inver" variable. The values of both "m" and "inver" are stored in cache within the specific new environment created when calling the function "makeCacheMatrix".
    set <- function(y) {
        m <<- y
        inver <<- NULL
    }
    
    print(m)
    
    # Points to the stored matrix and gets its value.
    get <- function() m
    
    # Within the environment created, updates the value of the inverse of the special matrix to the variable "inver".
    setinver <- function(solve) inver <<- solve
    
    # Points to the stored inverse of the matrix and gets it value.
    getinver <- function() inver

# Returns a list object within a particular environment where the matrix and its inverse are stored and can be accessed thanks to the scooping rules.
    list(set = set, get = get, setinver = setinver, getinver = getinver)

}


## -------------------------------------------

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# Arguments:
# x = list returned by the "makeCacheMatrix" function with the information (environment) regarding the special matrix computed in the previous function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        print(x)
        
        inver <- x$getinver()
    
        print(inver)
        # If the inverse of the matrix has been already calculated and it is stored in cache.
        if(!is.null(inver)) {
            message("Getting cached inverted matrix")
            return(inver)
        }
        
        ## If the inverse of the matrix has not be calculated before (and its value in cache is NULL):
        
        # Assign the value of the special matrix to the variable "dat"
        dat <- x$get()
        print(dat)
        
        # Calculates the inverse of the matrix and save it in the "inver" variable.
        inver <- solve(dat, ...)
        # Set the inverse value of the matrix to the environment created for this specific matrix so it can be stored in cache.
        x$setinver(inver)
        
        # Returns the inverse of the matrix
        inver
        
}

