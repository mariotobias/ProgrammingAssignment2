#Hey, thx for assessing my functions. Two things:
#I am very new to programming and "R".
#So please be critic in a constructive way! I am looking forward to your comments.

#My two functions are used together for a) caching a matrix and b) computing the inverse of a matrix.
#If it was already computed it is returning the cached data in order to save user time.
#To be honest, this function is a related the example with the necessary adaptions in order to make it work.

#This first function returns a list with 4 different functions, which can be called by the "cacheSolve".
#"neu" can be used to creat a new matrix, which then gives "x" the new value of "y" in the parent environment through the special operator "<<-"
#with name-of-the-matrix$get you can call the matrix
#set_inverse is important in the second function as well as get_inverse, which I will explain below.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  neu <- function(y=matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(variable) m <<- variable
  get_inverse <- function() m
  list(neu = neu, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


# If "a" is not zero, it means that the inverse of a matrix has already been calculated/set and the cached data 
# will be returned/called by the set_inverse function. So this part is checking whether the data is already available.
# If it is zero (we do not have any results), than a new function "data" (I think it is a function?!) is created,
# which calls the matrix from the function above.
# In "a", the inverse is calculated by the generic function "solve".
# Then comes the (most) interesting part, which I hope I got right although I do not exactly know how to express myself correctly
# "a" is the inverse and again function one and two are getting connected. The Inverse is set and gets passed into the first function
# and is stored in the above function. So if we called "a" now, we would get the answer "getting cached data"


cacheSolve <- function(x) {
  
  a <- x$get_inverse()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data)
  x$set_inverse(a)
  a
}
