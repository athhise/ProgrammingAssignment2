## Put comments here that give an overall description of what your
## functions do

###########################################################################
# The function makeCacheMatrix takes as input a matrix then return back a # 
# special matrix called SpecialMatrix.                                    #
# This SpecialMatrix come back with differents fonction that can:         #
#  - set the matric to other values and save it into the cache (set funct)#
#  - return the value of the initial matrix (get function)                #
#  - set its inverse to a certain value  and save it into the cache       #
#    (setInverse function)                                                #
#  - return the inverse of the initial matrix (getInverse function)       #
###########################################################################



## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse<<-NULL
        
        #set the matrix in the cache and initialize its inverse
        set<-function(matrice){
                x<<-matrice
                inverse<<-NULL
        }
        
        #Get the matrix from the cache
        get<-function() x
        
        #set the inverse of the matrix in the cache by a certain value inverse0
        setInverse<-function(inverse_real){
                inverse<<-inverse_real
        }
        
        #Get the inverse of the matrix back from the cache
        getInverse<-function() inverse
        
        #the function return a list of function
        specialMatrix<-list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
        return(specialMatrix)
}


## Write a short comment describing this function
##########################################################################
# cacheSolve must take into argument the specialMatrix from the previous #
# function. cacheSolve firstly check into the cache if the inverse of the#
# matrix given by specialMatrix have been already computed, if so then it#
# goes return it from the cache, if it is not the case, it compputes the #
# inverse of that matrix save it into the cache then return the inverse  #
#                                                                        #
##########################################################################


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #check if the inverse of the matrix was already compute and if it is in the cache
        inverse<-x$getInverse()
        if (!is.null(inverse)){
                message("Already computed. The inverse is:")
                return(inverse)
        }
        
        #compute the inverse as it hasn't been computed yet
        matriceToBeInverted<-x$get()
        inverse<-solve(matriceToBeInverted)
        #save the inverse into cache
        x$setInverse(inverse)
        return(inverse)
}
