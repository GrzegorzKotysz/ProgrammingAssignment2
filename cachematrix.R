## Function 'makeCacheMatrix' creates a list containing four functions and two 
## variables used to store matrix and its inverse. 
## Function 'cacheSolve' either calculates the inverse of matrix or 
## returns the value stored by 'makeCacheMatrix' 
## if the inverse has been already calculated.

## 'makeCacheMatrix' produces a list of four functions 
## ('set', 'get', 'setinv' and 'getinv') and stores two matrices.
## Usage:
## 'makeCacheMatrix(mx = matrix(), Safe = TRUE)'
## Arguments: 
## 'mx' - matrix which should be stored (may not be specified)
## 'safe' (default = TRUE) - determines whether function 'setinv' should operate
## in safe mode. This mode prevents having wrong matrix - inverse of the matrix
## couple. However, in this mode you cannot explicitly choose the matrix 
## to be stored as the inverse.
## Instead, the inverse of the matrix stored by 'set' function is calculated.
## 
## 'set(mx)' stores a matrix and removes the stored inverse to avoid confusion
## 'get()' displays the stored matrix
## 'setinv()' [in safe mode] calculates and stores the inverse of stored matrix
## 'setinv(inv)' [otherwise] stores chosen matrix as the inverse of 'mx' matrix
## 'getinv()' displays the stored inverse of a matrix

## Note: while in safe mode it is impossible to have wrong matrix - its inverse
## couple, it may happen in unsafe mode when a wrong matrix is stored as the inverse. 

makeCacheMatrix <- function(mx = matrix(), safe = TRUE) {
        inv <- NULL
        set <- function(mx){ #stores matrix
                mx <<- mx 
                inv <<- NULL 
                #overwrites inverse to assure that mx and inv are matching
        }
        get <- function(){ #returns stored matrix
                mx
        }
        if(safe){ #cannot manually store inverse
                setinv <- function(){ #calculates and stores inverse of matrix
                        inv <<- solve(mx)
                }
        } else { #can manually store inverse
                setinv <- function(inv){ #stores inverse of matrix
                        inv <<- inv
                }
        }
        getinv <- function(){ #returns stored inverse
                inv
        }
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## 'cacheSolve' calculates or displays the inverse of a chosen matrix depending
## on whether it was stored earlier in a chosen list.
## Usage:
## 'cacheSolve(x, mx)'
## Arguments:
## 'x' list created by makeCacheMatrix function
## 'mx' matrix whose inverse is to be calculated
##
## Note: if wrong inverse matrix is explicitly defined by 'setinv' function in
## unsafe mode and both, matrix stored by 'set' function and matrix used as a
## cacheSolve argument are the same, cacheSolve will display cached inverse, which
## threfore is false. In any other case no errors should be encountered.

cacheSolve <- function(x, mx) {
        inv <- x$getinv()
        smx <- x$get() #smx stands for stored matrix
        if(is.null(formals(x$setinv))){ #if safe mode is on
                if(!is.null(inv) & identical(smx, mx)) { #correct matrix stored
                        message("cached data returned") & return(inv)
                } else if(!is.null(inv) & !identical(smx, mx)){ #wrong matrix stored
                        message("dissimilar matrix stored, calculating inverse...")
                        x$set(mx)
                        x$setinv()
                        x$getinv()
                } else { #no inverse matrix saved
                        message("no cached data, calculating inverse...")
                        x$set(mx)
                        x$setinv()
                        x$getinv()
                }
        } else { #if safe mode is off
                if(!is.null(inv) & identical(smx, mx)) { #correct matrix stored
                        message("cached data returned") & return(inv)
                } else if(!is.null(inv) & !identical(smx, mx)){ #wrong matrix stored
                        message("dissimilar matrix stored, calculating inverse...")
                        x$set(mx)
                        x$setinv(solve(mx))
                        x$getinv()
                        
                } else { #no inverse matrix saved
                        message("no cached data, calculating inverse...")
                        x$set(mx)
                        x$setinv(solve(mx))
                        x$getinv()
                }
        }
}