## This script consists of two functions that helps to calculate the inverse of matrix and
## cache the value for later use. So that recomputation is neglected if 
## the inverse has already been calculated.

## This function has inner functions such as setMat, getMat, setInverse, getInverse.
## setMat - get the y as matrix argument and assigned to mat variable and make mat.inverse 'NULL'
## getMat - return the matirx (mat)
## setInverse - get the inv matrix and assigned it to mat.inverse variable
## getInverse - return the mat.inverse
## as a last statement function returns list of all inner functions named as set, get, setInv, getInv
makeCacheMatrix <- function(mat = matrix()) {
        mat.inverse <- NULL
        setMat <- function(y) {
                mat <<- y
                mat.inverse <<- NULL
        }
        
        getMat <- function() mat
        setInverse <- function(inv) {
                mat.inverse <<- inv
        }
        
        getInverse <- function() {
                mat.inverse
        }
        
        list(set = setMat, get = getMat,
             setInv = setInverse,
             getInv = getInverse)           
}


## This function calculate the inverse from given matrix if it has already not been cached
cacheSolve <- function(make.matrix, ...) {
        ## check whether the inverse is already cached
        if (!is.null(make.matrix$getInv())){
                message("Getting cached data...")
                return (make.matrix$getInv())
        }
        
        ## get matrix from given list to calcualte the inverse
        mat <- make.matrix$get()
        
        ## calcualte the inverse 
        mat.inverse <- solve(mat,...)
        
        ##set the inverse matrix 
        make.matrix$setInv(mat.inverse)
        
        ## return the inverse matrix
        mat.inverse
}

### Unit Test Cases 

##>    source("cachematrix.R")

##>    amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##>    amatrix$get()         # Returns original matrix
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4

## >   cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
##    [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## >  amatrix$getInv()  # Returns matrix inverse
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## >  cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
##Getting cached data...
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##>    amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
##>    cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
##            [,1] [,2]
##[1,] -0.13333333  0.2
##[2,]  0.01010101  0.0

##>    amatrix$get()         # Returns matrix
##     [,1] [,2]
##[1,]    0   99
##[2,]    5   66

##>    amatrix$getInv()  # Returns matrix inverse
##            [,1] [,2]
##[1,] -0.13333333  0.2
##[2,]  0.01010101  0.0
