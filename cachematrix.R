## Functions to cache the matrix and inverse it

## makeCacheMatrix provides a list of functions (get, set, getmatrix, setmatrix)

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
             x<<-y
             m<<-NULL
        }

        get<-function() x
        setmatrix<-function(z) m<<-z
        ## m is NULL if matrix not yet solved
        getmatrix<-function() m
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## cacheSolve inverse your matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
           message("read cached data")
           return(m)
        }
        
        ## vector is not cached, so need to inverse and set first
        
        data<-x$get()
        m<-solve(data, ...)
        x$setmatrix(m)
        m
}
