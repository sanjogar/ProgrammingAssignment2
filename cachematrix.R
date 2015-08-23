### Assignment 2. Caching the inverse of a matrix

# sanjogar
# Update: 22/08/2015


## -------------------------------------------
## GENERAL
# These two functions calculate the inverse of a matrix and store its value in cache.
# The functiom "makeCacheMatrix" generates the environment where the particular matrix and its inverse are stored. It returns a list object which is used as the argument of the function "cacheSolve".
# The "cacheSolve" function calculates the inverse of the matrix. First it checks if the inverse has been already calculated and it so, it retrieves the value stored in cache. If the inverse of that particular matrix has not been determined yet, then this function calculates it and makes it store in cache (within the environment created by the "makeCacheMatrix" function for the particular matrix).



## -------------------------------------------
## FIRST FUNCTION
# This function creates a new environment into which objects can be stored in cache (in this case a specific matrix "m" and its inverse "inver").
# The values of these two objects are then setted as well as the location of where they are stored in cache. Therefore, the matrix and its inverse can be called from another function trough the object (list) that this function returns.

# Arguments:
# m = specific matrix which value and its inverse are stored in cache.

# Example: mm <- makeCacheMatrix(m)

makeCacheMatrix <- function(m = matrix()) {

    inver <- NULL
    
    # Creating a new environment for the specific matrix.
    # Assigning the value of the matrix to the variable "m" and initialising the "inver" variable to NULL. The values of both "m" and "inver" are stored in cache within the specific new environment created for the matrix.
    set <- function(y) {
        m <<- y
        inver <<- NULL
    }
    
    # Points to the stored matrix and gets its value.
    get <- function() m
    
    # Updates the variable "inver" with the matrix's inverse value.
    setinver <- function(solve) inver <<- solve
    
    # Points to the inverse of the matrix stored in cache and gets it value.
    getinver <- function() inver
    
    # Returns a list object within a particular environment where the matrix and its inverse are stored, so they can be accessed thanks to the scooping rules.
    list(set = set, get = get, setinver = setinver, getinver = getinver)

}


## -------------------------------------------
## SECOND FUNCTION
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix function according to two cases:
#   1. If the inverse has already been calculated (and the matrix has not changed), then the "cacheSolve" function retrieves the inverse matrix stored in cache (and pointed through the list returned by the previous function).
#   2. If the inverse has NOT been calculculated, then this function determines the inverse of the matrix and makes it store in cache using the command "x$setinver(inver)".

# Arguments:
# x = list returned by the "makeCacheMatrix" function with the information (environment) regarding the computed matrix.

# Example: cacheSolve(mm)

cacheSolve <- function(x, ...) {
    
        # Retrieves from cache the inverse of the special matrix.
        inver <- x$getinver()
    
        ##
        # If the inverse of the matrix has been already calculated (i.e, inver != NULL) and it is stored in cache then:
        if(!is.null(inver)) {
            message("Getting cached inverted matrix")
            return(inver)
        }
        
        ##
        ## If the inverse of the matrix has not be calculated before (and its value in cache is NULL) then:
        
        # Assigns the value of the matrix to the variable "dat":
        dat <- x$get()
        
        # Calculates the inverse of the matrix "dat" and saves it in the variable "inver".
        inver <- solve(dat, ...)
        
        # Sets the value of the inverse of the matrix in order to be stored in cache (using the specific environment created for the special matrix by the previous function).
        x$setinver(inver)
        
        # Returns the inverse of the matrix.
        inver
        
}

