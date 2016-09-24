## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special matrix object than can cache its inverse

makeCacheMatrix <- function(x = matrix()) {## define x as a invertible square matrix
inv <- NULL
set <- function(y){## set the matrix
		x <<- y
		inv <<- NULL
}
		get<-function() x ## get the matrix
		setinverse <- function(inverse) inv <<- inverse ## set the inverse of the matrix
		getinverse <- function() inv ## get the inverse of the matrix
		list(set = set,
		     get = get,
	             setinverse = setinverse,
	             getinverse = getinverse) ## use this as input to cacheSolve
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already calculated (and the matrix has not changed), then the cachSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
			inv <- x$getinverse() ## if the inverse has already been calculated, then get the value of the inverse from cacheSolve
			if(!is.null(inv)) {
				message("getting cached data")
				return(inv)
			}
			data <- x$get() ## otherwise get/ calculate the value of the inverse from cacheSolve
			inv <- solve(data, ...)
			x$setinverse(inv) ##set the value of the inverse from cacheSolve
			inv
}

## Test codings
> matrix44<-matrix(sample(1:1000,16),ncol=4) ## Create a matrix (4x4) from the sample (1:1000)
> matrix44 ## Show the matrix (4x4)
     [,1] [,2] [,3] [,4]
[1,]  113  900  453  449
[2,]  740  210  698  356
[3,]   85  188  997  299
[4,]  484  530  281  570
> mcm44<-makeCacheMatrix(matrix44) ## Assign makeCacheMatrix to mcm44
> mcm44$get() ##get the value of mcm44
     [,1] [,2] [,3] [,4]
[1,]  113  900  453  449
[2,]  740  210  698  356
[3,]   85  188  997  299
[4,]  484  530  281  570
> cacheSolve(mcm44) ## Calculate the value of cacheSolve
       [,1]          [,2]          [,3]          [,4]
[1,]  0.0002525096  0.0019500623 -0.0012681267 -0.0007516300
[2,]  0.0020828389  0.0008545817 -0.0010934685 -0.0016008398
[3,]  0.0002708784  0.0004781895  0.0008090855 -0.0009364499
[4,] -0.0022846247 -0.0026861925  0.0016946642  0.0043427657
