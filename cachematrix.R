
#This function allocates the value of an inverse matrix in the cache memory. 
#A matrix is given as an argument and a list of functions is created, when the original function is called
# it retrieves a set of funtions and each can be called with the subset operator $ . 
#Set() or Setinv() will allocate the given matrices in the parent environment. 
#Get() and Getinv() can retrieve them. 

makeCacheMatrix <- function(x = matrix(),...) { # I added the ldots arguments since matrix requieres add arguments
        inv <- NULL
        set <- function(y,...) { #y is a matrix type
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(y,...) {#y is a matrix type
                inv <<- y
        }
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## This function takes the inverse of a matrix if it is not allocated in the cache memmory, if it is 
#the function will retrieve its value from the parent environment. It takes as an argument a function type 
#as the above defined.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
