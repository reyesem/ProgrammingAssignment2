################################################################################
# Program: cachematrix
# Description: Set of two functions which computes the inverse of a matrix.
#              If the inverse has already been computed, then it is retrieved
#              from cache. Otherwise, the inverse is computed and cached for
#              future use. Cache is implemented through attributes.
#
# Author: Eric Reyes
# Date: March 2016
#
# Notes:
#  1. Completed in partial fulfillment of R Programming course as part of the
#      Data Science specialization on Coursera.



# function: makeCacheMatrix
# description: Adds an attribute to a matrix which contains the inverse of the
#              matrix.
#
# parameters:
#  x           Matrix. The matrix of which to compute the inverse.
#  ...         Additional parameters to pass to solve().
#
# notes:
#  per the programming instructions, it is assumed that x has an inverse.
makeCacheMatrix <- function(x = matrix(), ...) {
  # add attribute to x which contains the inverse.
  attr(x, "inverse") <- solve(x, ...)
  
  # return matrix
  return(x)
}


# function: cacheSolve
# description: Implementation of solve() function in base R that computes the
#              inverse of a matrix. It first checks to see if the inverse has
#              already been computed and stored as an attribute to x. If not,
#              then the attribute is added for future use. If so, the inverse
#              is grabbed instead of recomputed.
#
# parameters:
#  x           Matrix. The matrix of which to compute the inverse.
#  ...         Additional parameters to pass to solve.
cacheSolve <- function(x, ...) {
  # if inverse exists, return it.
  if(!is.null(attr(x, "inverse"))) return(attr(x, "inverse"))
  
  # Otherwise, compute inverse and cache it for future use
  else{
    # construct inverse
    new.x <- makeCacheMatrix(x, ...)
    
    # cache it for future use
    eval.parent(substitute(x <- new.x))
    
    # return inverse
    return(attr(new.x, "inverse"))
  }
}
