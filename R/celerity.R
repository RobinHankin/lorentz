`cel_to_vel` <- function(x){sweep(as.matrix(x),1,beta(x),"/")} # returns 3velocity, given 3celerity
`vel_to_cel` <- function(x){sweep(as.matrix(x),1,gam (x),"*")} # returns 3celerity, given 3velocity
