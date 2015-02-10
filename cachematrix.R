##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize the inverse property
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    
    ##How to get the matrix
    get<-function() x
    
    ##How to set the inverse of the matrix
    setmatrix<-function(solve) m<<- solve
    
    ##How to get the inverse of the matrix
    getmatrix<-function() m
    
    ##list of the methods
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}
##This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {

    ##Return a matrix that is the inverse of 'x'.
    ##If it has already been calculated, it gets it from the cache
    ##and skips the calculation
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    ##If the inverse doesn't exsist, this function calculates it
    ##and stores it in cache
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
