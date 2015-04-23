## In this file we construct two functions.  The first function, makeCacheMatrix, 
## is a sort of matrix object which caches its inverse. This is executed in the 
## form of a list of functions.  The second function takes an instance of the first
## as an argument; if the instance has a cached inverse, this inverse is called
## If not, an inverse is calculated and cached.

## This function produces a list of 4 functions, which in conjunction will act
##like a matrix which can keep track of a cached inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL #inv is short for inverse.  It defaults to NULL
    set <- function(y) { #This function lets us change x.  If we do, we also
        x <<- y          # reset inv to be NULL, bc the cached value will not  
        inv <<- NULL     # be the inverse of our new matrix
    }
    get <- function() {x} #returns the matrix x
    setinv <- function(inverse) {inv <<- inverse} #sets inv to be equal to the
                        #argument inverse. In practice We want the argument to be
                        #the inverse of x
    
    getinv <- function() {inv} #returns the value of inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
   
}
## This function works in conjunction with makeCacheMatrix.  If an inverse is 
## already bein stored, it returns a message and the inverse.  If not, an inverse
# is calculated and then stored in the makeCacheMatrix variable. 
cacheSolve <- function(x, ...) {
        
    inv <- x$getinv()
    if(!is.null(inv)) { #Recall that inv defaulted to NULL.  If it is not NULL
        #then we return whatever value is stored
        
        message("getting cached inverse")
        return(inv) #we return the inverse and exit the function
    }
    # if we are here then inv=NULL.  We want to solve for the inverse and 
    # assign this to inv
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    
}
