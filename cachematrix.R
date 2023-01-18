## coursera R Programming Assignment 2 - Lexical Scoping     J. Maxey 01-18-2023

## This pair of functions take a square matrix and create its inverse
## The inverse is stored in the environment of the function in which it
## is created so that it can be retrieved without the cpu expense of recreating 
## it. A square matrix is required as input.

## Function 1 - makeCacheMatix 
## Creates an R object that stores a matrix and its inverse
## The object is a "list" containing functions to set and get values stored
## in the functions creating environment

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(inputMatrix){
        x <<- inputMatrix
        inverseMatrix <<- NULL
    } 
    get <- function() x
    setInverse <- function(solveMatrix) inverseMatrix <<- solveMatrix
    getInverse <- function()inverseMatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function 2 - cacheSolve
## takes as an argument the R object created by Funtion 1
## First calls the "get" function to load the stored value of the inverse of inputMatrix
## If the stored value is not NULL, a message is displayed and the stored value is shown
## If the stored value is NULL the set function is called to create a new inverse of
## inputMatrix which is then shown

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)){
        message("\nValue of inverseMatrix exists from prior run")
        message("No need for recalulation, echoing prior value:\n")
        return(inverseMatrix)
    }
    else
    {
    message("\nValue of inverseMatrix is NULL - calculating new value:\n")
    data <- x$get()
    inverseMatrix <- solve(data)
    x$setInverse(inverseMatrix)
    inverseMatrix 
    }
}
