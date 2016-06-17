## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
