#######################################################
#These two functions demonstrate loading and retrieving
#cached values
#
#Example run commands
#> a<-makeCacheMatrix()
#> a$setMatrix(matrix(1:4,2,2))
#> cacheSolve(a)
#
#######################################################

#This function creates the cache and the setters and getters
makeCacheMatrix <- function(m = matrix()) {
        #initialize
        m<-NULL
        x<-NULL
        
        #setter - matrix cache
        setMx<-function(m){
                m<<-m
        }
        
        #getter - matrix
        getMx<-function() m
        
        #setter - inverse matrix cache
        setIMx<-function(x) {
                x<<-x
        }
        
        #getter - inverse matrix
        getIMx<-function() x
        
        #return the getters and setters as a list
        list(setMatrix=setMx, 
             getMatrix=getMx, 
             setInvMatrix=setIMx,
             getInvMatrix=getIMx)
}

#This function sets and gets the cached values of the inverted matrix
cacheSolve <- function(x=matrix()) {
        #get inverse matrix
        imx<-x$getInvMatrix()
        
        #if its not in the cache load it
        if(is.null(imx)){
                message("Loaded into cache.")
                #load the inverse cache
                imx<-x$setInvMatrix(solve(x$getMatrix()))
        } 
        
        #return cached value
        imx
        
}