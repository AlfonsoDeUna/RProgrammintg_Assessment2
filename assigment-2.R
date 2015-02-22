#cache matrix
makeCacheMatrix <- function(x = numeric()) {
  cache <- NULL
  
  set <- function(nuevo) {
    x <<- nuevo
    # Cuando en la caché añadimos valores vaciamos
    cache <<- NULL
  }
  get <- function() { x }
  setR <- function(r) { cache <<- r }
  getR <- function() { m }
  list(set = set, get = get, setR = setR,
       getR = getR)
}

#inverse matrix
cacheSolve <- function(y, ...) {
  # obtenemos el valor cache
  valorcache <- y$getR()
  # if a cached value exists return it
  if(!is.null(valorcache)) {
    message("get value cache ")
    return(valorcache)
  }
  
  
  data <- y$ge()
  cachevalue <- solve(data)
  y$setR(inverse)
  
  # return value cache
  valorcache
}