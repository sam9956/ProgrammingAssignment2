makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
# Define the matrix
my_matrix <- matrix(c(2, 3, 1, 4), 2, 2)
print("Original Matrix:")
[1] "Original Matrix:"
> print(my_matrix)
[,1] [,2]
[1,]    2    1
[2,]    3    4

# Use makeCacheMatrix to convert it to a special matrix object
cachedMatrix <- makeCacheMatrix(my_matrix)

# Compute the inverse of the matrix for the first time (this will not use the cache)
inverse1 <- cacheSolve(cachedMatrix)
print("Inverse of Matrix (First Computation):")
print(inverse1)
[,1] [,2]
[1,]  0.8 -0.2
[2,] -0.6  0.4


