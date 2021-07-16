`3cel_to_3vel` <- function(x){sweep(x,1,zet(x),"/")} # returns 3velocity, given 3celerity
`vel_to_cel` <- function(x){sweep(x,1,gam(x),"/")} # returns 3celerity, given 3velocity

