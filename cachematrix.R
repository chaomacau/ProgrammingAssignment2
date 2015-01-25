##Doing a Matrix inversion with cacheing.

## makeCacheMatrix creates a matrix object that its inverse can be cached.

makeCacheMatrix <- function(A = matrix()) {
        Inv <- NULL
        set <- function(M) {
                A <<- M
                Inv <<- NULL
        }
        get <- function() A
        setInv <- function(invmat) Inv <<-invmat
        getInv<- function() Inv
        list(set= set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix if its inverse has not been cached.
## It retrieves the inverse from the cache if inverse of the matrix has been calculated. 

cacheSolve <- function(A, ...) {
        Inv <- A$getInv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        Mat <- A$get()
        Inv <- solve(Mat, ...)
        A$setInv(Inv)
        Inv
}