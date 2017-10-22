
 +#1st function
  makeCacheMatrix <- function(x = matrix()) {
 -  m<-NULL
 -  set<-function(y){
 -  x<<-y
 -  m<<-NULL
 -}
 -get<-function() x
 -setmatrix<-function(solve) m<<- solve
 -getmatrix<-function() m
 -list(set=set, get=get,
 -   setmatrix=setmatrix,
 -   getmatrix=getmatrix)
 +        i <- NULL
 +        set <- function(y) {
 +                x <<- y
 +                i <<- NULL
 +        }
 +        get <- function() x
 +        setinverse <- function(inverse) i <<- inverse
 +        getinverse <- function() i
 +        list(set = set,
 +             get = get,
 +             setinverse = setinverse,
 +             getinverse = getinverse)
  }
  
 -cacheSolve <- function(x=matrix(), ...) {
 -    m<-x$getmatrix()
 -    if(!is.null(m)){
 -      message("getting cached data")
 -      return(m)
 -    }
 -    datos<-x$get()
 -    m<-solve(datos, ...)
 -    x$setmatrix(m)
 -    m
 +#2nd function
 +
 +cacheSolve <- function(x, ...) {
 +        i <- x$getinverse()
 +        if (!is.null(i)) {
 +                message("getting cached data")
 +                return(i)
 +        }
 +        data <- x$get()
 +        i <- solve(data, ...)
 +        x$setinverse(i)
 +        i
  }
 +
 +# to prove our code we built up a matrix
 +M <- matrix(c(1,2,3,4),2,2)
 +
 +#Step1. To get the inverse matrix but without being computed yet
 +M1 <- makeCacheMatrix(M)
 +  
 +#Step2. Get the inverse matrix after the computation
 +cacheSolve(M1) 
 +
 +#Step3. Recall the inversed matrix from cache. 
 +cacheSolve(M1) 
 +  }
