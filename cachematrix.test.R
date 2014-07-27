## Test our cached inverse implementation in cachematrix.R against 
## uncached inverse computation.
source('cachematrix.R')
test <- function() {
    M <- matrix(c(1,2,3,4),nrow=2,ncol=2)
    z <- makeCacheMatrix(M)
    
    print(system.time(for(i in 1:1000) {cacheSolve(z)}))
    print(system.time(for(i in 1:1000) {solve(M)}))
}