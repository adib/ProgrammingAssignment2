## Programming Assignment 2 – Cacheable Matrix 👍
## Contains functions to create a matrix with cacheable inverse and calculate the inverse.
## 
## Part of R Programming course in Coursera
## by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD

#' Creates an object that contains a matrix and a placeholder for its inverse. 
#' @param m the initial value of the matrix
#' @return a list containing these functions:
#'    - `set(x)`  re-sets the value of the matrix to the value passed
#'    - `get`     returns the value of the matrix.
#'    - `setInverse(x)` sets the value of the matrix's inverse
#'    - `getInverse`    returns the value of the matrix's inverse.
makeCacheMatrix <- function(m = matrix()) {
    matrixInverse <- NULL
    matrixValue <- m
    set <- function(v) {
        if(any(matrixValue != v)) {
            # Invalidate the inverse since we're re-setting the matrix's value
            matrixInverse <<- NULL
            matrixValue <<- v
        }
    }
    get <- function() {
        matrixValue
    }
    setInverse <- function(v) {
        matrixInverse <<- v
    }
    getInverse <- function() {
        matrixInverse
    }
    list(get=get, set=set, getInverse=getInverse, setInverse=setInverse )
}


#' Calculates a cacheable matrix's inverse and returns its value.
#' This function will cache the calculated inverse and will use that cached value if available instead of calculating the inverse.
#' @param m A cacheable matrix returned by function `makeCacheMatrix`
#' @return The cacheable matrix's inverse.
cacheSolve <- function(m, ...) {
    i <- m$getInverse()
    if(is.null(i)) {
        i <- solve(m$get())
        m$setInverse(i)
    }
    i
}
