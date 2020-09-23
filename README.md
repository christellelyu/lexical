# lexical
makeCacheMatrix <- function(x = matrix()) {
        # initialize m to be NULL when first call this function
        m <- NULL
        # print the environment and parent environment
        print(environment())
        evn <- environment()
        print(parent.env(evn))
        # set x and inv
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # fetch the value of x and return
        get <- function() x
        # pass the value of inv to m (into cache)
        setinv <- function(inv) m <<- inv
        # fetch the value of m from cache
        getinv <- function() m
        # get the environment value
        getevn<- function() environment()
        # list the elements
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv,
             getevn = getevn)
        
}


## This function computes the inverse of given matrix: if there is any value that was in 
## the cache, fetch it directly; otherwise compute the matrix inverse again.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        #print(length(m))
        # if there is an 'm' in the cache, fetch it directly from cache
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        # othereise get the original x and compute its inverse
        data <- x$get()
        m <- solve(data, ...)
        # set inverse in cache
        x$setinv(m)
        m
}

###############################################
## Test code:
## Run the following part:
# input value from x
x <- matrix(c(-1, -2, 1, 1), 2,2)
# fresh the cache
mat <- makeCacheMatrix(x)
# solve mat for the 1st time
cacheSolve(mat)
# solve mat for the 2nd time (fetch from cache)
cacheSolve(mat)
