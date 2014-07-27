makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    message("getting fresh data")
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

main <- function() {
    a <- makeVector()
    print(a$get())
    
    a$set(c(0:1,10000,replace=TRUE))
    #print(a$get())
    
    
    print("Set1:")
    stime <- system.time(m <<- cachemean(a),gcFirst = FALSE)
    print(m)
    print(stime)
    
    stime <- system.time(m <<- cachemean(a),gcFirst = FALSE)
    print(m)
    print(stime)
    
    a$set(sample(0:1,100000,replace=TRUE))
    #print(a$get())
    
    print("Set2:")
    stime <- system.time(m <<- cachemean(a),gcFirst = FALSE)
    print(m)
    print(stime)

    stime <- system.time(m <<- cachemean(a),gcFirst = FALSE)
    print(m)
    print(stime)
    
}