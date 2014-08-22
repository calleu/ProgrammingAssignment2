## This function's object is to save the time in solving the inverse of the 
## specific matrix, adopting the mehod that calculating and cache the inverse 
## solution at the first time, then directly obtain the solution when was called
## again. Thanks the contribution from Bill Hilton, whoes thread help me understand
## the principle of those examples.

makeCacheMatrix <- function(x = matrix()) {   
        ##define the function to make the list cache the solution
        
        s<- NULL
        ## every time the list was created, it will set the flag indicate 
        ## it contains no solution.
        
        set <- function(y = matrix()){   ##the function just to reset the list bypassingly
                s <<- NULL
                x <<- y
        }
        get <- function(){x}
        ## define the object to return the matrix. Why? When the cacheSolve function find
        ## out that the list doesn't contain the solution, then it need to get the matrix
        ## to perform solving function at once.
        
        setsolve <-function(solve) {s <<- solve}
        ## when the cacheSolve function done the calculation, it would call this sub-
        ## function to save the solution in setsolve of the list.
        ## the <<- can transmit the value to the "s", which was defined in parant function
        ## other than the setsolve function.
        
        getsolve <-function(){s}
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}
## to create the list contain four objects, or we can say four methods, those with
## different usage.




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', by visit the list above, seeing 
        ## whether the list contains the solution, if no, cacluate it, and save it in the
        ## list; if yes, return it directly, which can obviously save the time when solve
        ## large data.
        
        s <-x$getsolve()
        ## to get the value in the list "x"
        
        if(!is.null(s)){
                ## to judge whether "s" a NULL, if it's not a NULL, then return s as solution
                ## at once.
                
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        ## if s is a NULL, meaning the solution has not yet been solved, then call the
        ## list to get the matrix
        
        s <- solve(data,...)
        ## solve it
        
        x$setsolve(s)
        ## save the result in "s", using setsolve method contain in x
        
        s
        ##return the result
}

## the process of functions are: Get the matrix to create a list, saving it. when
## the list was called at first time, it would tell the cache function it contains
## only matrix, not the solution, then the cache function would get the data and
## solve it, return the solution to save in the list. When the list was called
## again, it can tell the function that the solution is already ready, which just
## need to be fetched directly, saving the time and resource to calculte it repeatly.
## Thanks Bill Hilton again!
