## There are two functions in this file. They are makeCacheMatrix and cacheSolve.
## The purpose or description is written just before each function.



## The below function creates a matrix by "cacheing" the inverse of this matrix


makeCacheMatrix <- function(mmt = Matrix())	{

inv<-NULL

set<-function(x)	{

mmt<<-x

inv<<-NULL

}


get<-function() mmt

setinv<-function(inverse) inv <<- inverse 

getinv<-function() inv 



list(set = set, get = get, setinv = setinv, getinv = inv)

}





## The below function produces the inverse of the "matrix" returned by makeCacheMatrix above.
## If the inverse has already been produced,
## it gets the inverse from the cache and skips the computation.
## If not, it produces the inverse of the matrix,
## and sets the value of the inverse.


cacheSolve<-function(mmt, ...) {

inv<-mmt$getinv() 
 
if (!is.null(inv)) { 
message("getting cached data") 
return(inv) 
} 


data<-mmt$get() 
inv<-inv(data, ...) 
 

mmt$setinv(inv)
inv 
        
}
