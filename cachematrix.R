## Put comments here that give an overall description of what your


## Creating a placeholder environment and creating a cache for a matrix object

makeCacheMatrix <- function(x=numeric(),r,c) {  ## x is a atomic vector, r=rows, c=columns
  
  if (r!=c) return(message("Not a Square Matrix!")) ## Testing for square matrix
  if (length(x)!=r*c) return(message("Less/Excess Data to Accomodate!"))  ##Matching size of data and matrix
  IM<-matrix(0,r,c)         ## Initializing a matrix cache to hold inverse
  dim(x)<-c(r,c)            ## Converting atomic vector x into matrix for size r,c
  getdata <- function() x   ## Getting/Reading given data as matrix
  getIM<-function() IM      ## Reading matrix cache
  
  list(getIM=getIM,getdata=getdata) ## Returning a list of functions
    
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x,...) {         
  IM<-x$getIM()                  ## Reading matrix cache

  if (sum(IM)!=0) {              ## Checking if cache is empty
    message("Getting Inverse from Cache")
    return(IM)                   ## Returning cached data
  }
  m<-x$getdata()                 ## Getting/Reading data in object environment
  IM<<-solve(m)                  ## Returning inverse of matrix
    
}
