#' @description This module holds a function to make a matrix of which the inverse calculation can be cached.
#' @details A matrix has to be created using the @seealso makeCacheMatrix function
#'          and the matrix inversion can be retrieved with the @seealso cacheSolve function.
#' @author Andreas Lehner

#' @description Create a new matrix which is cacheable.
#' @param x The input matrix which will be made cachable
#' @return A list containing the getter and setter for the matrix and its cached inversion
makeCacheMatrix <- function(x = matrix())
{
    # cached inversion
    cachedInvertedMatrix <- NULL
    
    # set a matrix
    set <- function(newMatrix)
    {
        x <<- newMatrix
        cachedInvertedMatrix <<- NULL
    }
    
    # return the matrix
    get <- function() return(x)
    
    # set the inverted matrix
    setInversion <- function(inversion) cachedInvertedMatrix <<- inversion
    
    # return the inverted matrix
    getinversion <- function() return(cachedInvertedMatrix)
    
    # return a list containing all the functions
    return(list(set = set, get = get, setInversion = setInversion, getinversion = getinversion))
}


#' @description Solves the inverse of the matrix.
#' @param x The input cacheable input matrix created with @seealso makeCacheMatrix
#' @return The inverse of the matrix x
cacheSolve <- function(x, ...)
{
    # get the current inversion
    inv <- x$getinversion()
    
    # check if a cached value is available
    if (!is.null(inv))
    {
        # if the cached value is available, return it
        return(inv)
    }
    
    # solve the matrix if it is not available
    inv <- solve(x$get(), ...)
    
    # set the inversion to make the cached value available for next use
    x$setInversion(inv)
    
    # return the inversion
    return(inv)
}
