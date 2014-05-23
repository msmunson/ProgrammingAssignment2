## These functions construct a matrix object which can calculate the value of
#the inverse of a matrix and cashe that value for retrieval. 

## makeCacheMatrix creates a matrix object consisting of four elements
## 1: set a function which sets the value of the matrix
## 2: get a function to retrieve the value of the matrix
## 3: setinverse a function to store the value of the matrix inverse
## 4: getinverse a function to retrieve the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
     #When the function is called, clear the inverse
     inverse<-NULL
     
     #Define the set function sets the matrix (and clears the inverse)
     set<-function(y){
          x<<-y
          inverse<<-NULL
     }
     
     #Define the get function
     get<- function() x
     
     #Set the inverse
     setinverse <- function(I) inverse <<- I
     
     #Retrieve the inverse
     getinverse <- function() inverse
     
     #Return a list of the functions
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks to see if the matrix that has been supplied as input
## to makeCacheMatrix has been inverted, if not, the inverse is calculated
## and cached. If it already has been calculated, it simply retrieves the 
## cached value.

cacheSolve <- function(x, ...) {
     #Retrieve the inverse of matrix from makeCacheMatrix
     I <- x$getinverse()
     
     #If the inverse exists, notify user and return the inverse
     if(!is.null(I)){
          message("Inverse already calculated. Retrieving...")
          return(I)
     }
     #Otherwise, get the original matrix
     data <- x$get()
     #Calculate the inverse
     I <-solve(data)
     #Write the calculated inverse to the cache in makeCacheMatrix
     x$setinverse(I)
     #Return the inverse
     I
}
