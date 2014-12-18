## These functions cache the inverse of a matrix

## This first function creates a special "matrix" object that can cache its 
## inverse

makeCacheMatrix <- function(x=matrix()) {
        s<-NULL  # s will be 'solve' and is set to NULL every time 
                 # makeCachMatrix() is called
        set<-function(y){ #takes input of matrix
                x<-y  #saves input of matrix
                s<-NULL  #Resets 'solve' to NULL when new objects are generated
        }
        get<-function(){x} #returns value of input
        setSolve<-function(solve) {s<<-solve}   # Called by cacheSolve() during 
                                                # first cache access and stores
                                                # s value via superassignment
        getSolve<-function(){s} # Returns cahced value to cacheSolve() for 
                                # future accesses 
        list(set=set, get=get, setSolve=setSolve, getSolve=getSolve) 
                                # List of internal functions accessed each time 
                                # makeCacheMatrix() is called
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) { # The input of x is created by makeCacheMatrix
        s<-x$getSolve() # Accesses object 'x" and gets value of inverse if mean
                        # was already cached (not NULL)
        if(!is.null(s)){
                message("getting cached data")  # Sends message to console
                return(s)                       # and returns inverse...
                                                # 'return' ends note for 
                                                # cacheSolve() function
        }
        data<-x$get()  # Get this code only if x$getSolve() returned NULL
        s<-solve(data,...) # If s was NULL, we have to calculate matrix inverse
        x$setSolve(s)  # Store the calculated matrix inverse value in s (see
                       # setSolve() in makeCacheMatrix)
        s              # Return matrix inverse to the code that called function
        
}
