################################################################################
## Two functons are there namely makeCacheMatrix() and cacheSolve(). These
## functions show how a computationally expensive vector could be cached for
## latter exploiting scoping rule of R language. Here, the first function
## creates a matrix and calculates inverse of it when it is first created and
## also whenever it is modified. The inverse matrix is cached for later use.
## Whenever user asks for the inverse of the matrix, it returns the inverse from
## the cache instead of calculating it again. The second function takes help of
## the first one. It create a matrix using makeCacheMatrix() function and later
## on asks for the inverse of the matrix. And also returns the inverse to the
## caller.
##
## Note: This example is somewhat different from the example given in the
## course site. In this example, the moment the matrix is modified, its inverse
## gets calculated instantly. When ever the inverse is asked for at the later
## stage, it is always returned from the cache. But in the example given in the
## course site, the moment the value of x is modified, the mean is reset to
## NULL. And, if the mean is asked after setting the value, it is first
## calculated and then returned. If the value of x is not changed, mean is
## returned from the cache.
################################################################################

################################################################################
## This function creates a matrix and its inverse and also defines sub-functions
## to manipulate on the matrix. Calculates inverse of the matrix when it is
## first created and when ever the matrix is modified. Caches the inverse matrix
## in the environment of the function. Returns the cached inverse when
## getInverse() is called.
## Exposed functions are as follows:
##      setMatrix() : Sets the matrix
##      setElement(): Sets individual elements of the matrix
##      getMatrix() : returns the matrix
##      getInverse(): returns the inverse of the matrix
##
##
## Input object(s):
##      x: input matrix, an optional parameter
##
## Return object:
##      a list all sub-functions exposed to outer world
##
## Warning:
##      Please note that this function will not work if a vector other than
##      a non-singular square matrix is passed as input
################################################################################
makeCacheMatrix <- function( x = matrix() ) {
    
    # Calculates the inverse, asumes that the input matrix is always a square and
    # non singular matrix
    calcInverse <- function() {
        inverse_x <<- solve( x )
    }
    
    # Sets matrix, checks for valid and square matrix. Automatically
    # re-calculates the inverse if the new matrix is modified. Returns
    # NULL if unable to set the matrix, otherwise returns the matrix
    setMatrix <- function( mat ) {
        
        # Validate the input matrix
        if( !is.matrix( mat ) ) {    # VAlid matrix?
            print( "Set fail: not a matrix" )
            return( NULL )
        }
        
        ## It could have been better if we would add checking of invertible
        ## matrix before going for inverse calculation. But, let us at this
        ## point assume that user will provide a square matrix.
        ## if( nrow( mat ) != ncol( mat ) ) {    # square matrix?
        ##     print( "Set fail: not a square matrix" )
        ##     return( NULL )
        ## }
        
        # Skip if the new matrix is identical with the
        # previous, ne need to calculate the inverse then
        if( identical( x, mat ) ) {
            print( "Warning: identical with previous matrix" )
            return( x )
        }
        
        # Set the matrix
        x <<- mat
        
        # Calculate inverse and cache in parent environment
        calcInverse()
    }
    
    # Set individual element of the matrix, automatically re-calculates the
    # inverse if the element is changed. Returns the new value.
    setElement <- function( row, col, val ) {
        if( is.na( x[row, col] ) || x[row, col] != val ) {
            x[row, col] <<- val
            
            # The matrix has changes. So, re-calculate inverse matrix
            calcInverse()
        }
        
        # Return the value itself
        return( val )
    }
    
    # Returns the matrix to the outer world
    getMatrix <- function() {
        x
    }
    
    # Retuns the inverse of the matrix
    getInverse <- function() {
        inverse_x
    }
    
    # Calculate inverse. Let us assume here that user will provide a valid
    # non-singular square matrix. So, no need to validate the matrix at this
    # point
    calcInverse()
    
    # Return the list of all the functions
    list( getMatrix = getMatrix, getInverse = getInverse,
          setElement = setElement, setMatrix  = setMatrix )
}


################################################################################
## This function takes input of a matrix and retuns it's inverse. Assumes that
## the input matrix is a square and non-singular one.
##
##
## Input object(s):
##      x: input matrix, a valid non-singular square matrix
##
## Return object:
##      Inverse of input matrix x
##
## Warning:
##      Please note that this function will not work if a vector other than
##      a non-singular square matrix is passed as input
################################################################################
cacheSolve <- function( x, ... ) {
    
    # Create the matrix and calculate the inverse of it and cache the inverse
    # for latter use. makeCacheMatrix() creates the matrix and automatically
    # calculates the inverse and cosecuently caches it for later use.
    matrix <- makeCacheMatrix( x )
    
    # Get the inverse of the matrix from the cache and return it
    matrix$getInverse()
}