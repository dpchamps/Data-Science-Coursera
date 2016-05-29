#makeCacheMatrix
# - Takes a matrix as an argument and returns a list of functions for preforming get 
# - and set operations on the matrix and it's inverse. 
# - By default, the matrix is cached and it's inverse is set to NULL;
# - When the inverse is calculated, setInverse can be called to cache the inverse of
# - the matrix as well.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL;
  set <- function(v){
    x <<- v;
    inverse <<- NULL;
  }
  get <- function(){
    x;
  }
  setInverse <- function(solve){
    inverse <<- solve;
  }
  getInverse <- function(){
    inverse;
  }
  list(
    get = get,
    set = set,
    setInverse = setInverse,
    getInverse = getInverse
  );
}
#cacheSolve
# - Expects a makeCacheMatrix object as argument x, which contains a square invertible matrix.
# - If the x$getInverse returns NULL, the inverse is calculated and then cached.
# - Otherwise, the cached inverse of the matrix stored in the object is returned.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse();
  if(!is.null(inverse)){
    return(inverse);
  }
  m <- x$get();
  inverse <- solve(m, ...);
  x$setInverse(inverse);
  inverse;
}