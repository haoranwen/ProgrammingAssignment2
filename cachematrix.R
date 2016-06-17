## The following two functions are used for computing and store the 
## inverse of any matrix. 

## the makeCacheMatrix function is used to build a special "matrix"
## with four subfunction in it. So that we can store and set the matrix and 
## its inverse into this object.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    get<-function()x
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    setinv<-function(inverse)inv<<-inverse
    getinv<-function()inv
    list(get=get,set=set,setinv=setinv,getinv=getinv)

}


## cacheSolve function is used to retrieve/calculate the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse<-x$getinv()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    matrix<-x$get()
    inverse<-solve(matrix)
    x$setinv(inverse)
    inverse
}
