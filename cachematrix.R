##  makeCacheMatrix  ##
##
## Takes an invertible square matrix, sets and caches value of its inverse.  Initializes 
## inverse value to NULL, until value is calculated via the setinv method
## returns a list object of '3 methods' to access the initial matrix and its inverse
## value

makeCacheMatrix <- function(x = matrix()) {  # input will be an invertible matrix  
    mtx_inv <- NULL                          # the inverted matrix supplied as 'x' above, initializes as NULL
    
    get <- function() { x }            # returns value of the originally supplied matrix
    setinv <- function(invert) {       # "set method" - called by cacheSolve() during initial cacheSolve() call
        mtx_inv <<- invert }           #                and stores value using superassignment ("<<-" operator)
    getinv <- function() { mtx_inv }   # "get method" - returns value of mtx_inv when called
    list(get = get,                    #  the list object returned by this function to the calling function/code
         setinv = setinv,
         getinv = getinv)
}


##  cacheSolve  ##
##
## Uses object created by makeCacheMatrix and calculates its inverse (if inverse not previously calculated)
## or retrieves cached inverse


cacheSolve <- function(mCM_obj, ...) { # mCM_obj is list object created by a previous call to makeCacheMatrix
    invtd_mtx <- mCM_obj$getinv()      # gets the value of the inverted matrix as stored by makeCacheMatrix
    if(!is.null(invtd_mtx)) {          # if matrix already inverted (i.e. not NULL)
        message("getting cached data") # message to console
        return(invtd_mtx)              # return value of cached inverted matrix as stored by makeCacheMatrix
                                       # Function terminates here **IF** an inverted matrix  has already been 
                                       # stored by makeCacheMatrix
    }
    data <- mCM_obj$get()              # **IF** makeCacheMatrix has NOT previously stored an inverted matrix value
                                       # this section of code runs - because invtd_mtx is NULL
    invtd_mtx <- solve(data, ...)      # calculates a value for the inverted matrix, using object created by
                                       # previous call to makeCacheMatrix
    mCM_obj$setinv(invtd_mtx)          # stores (caches) the value of the inverted matrix
    invtd_mtx                          # returns the value of the inverted matrix to the calling code
}
