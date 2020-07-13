makeCacheMatrix <- function(x = matrix()) { ## return the matrix value
        q <- NULL
        set <- function(y) { ##set a value
                x <<- y ##<<- assign a value to an object in an environment
                        ##that is different from the current environment
                q <<- NULL
        }
        get <- function() {x} ##get the set value
        setInv <- function(inverse) {q <<- inverse}
        getInv <- function() {q}##
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

cacheSolve <- function(x, ...) {
        q <- x$getInv() ## return the inverse matrix 
        if(!is.null(q)) {
                message("getting cached data")
                return(q) ## if already calculated, will return the data
        } 
        data <- x$get()
        q <- solve(data, ...)
        x$setInv(q)
        q
}