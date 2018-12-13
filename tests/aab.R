## Tests of basic SR relationships, eg U.U=-c^2 for a four-velocity.


require("lorentz")

checker1 <- function(U){is.consistent.4vel(U)}

`quad.form` <- function(M,x){crossprod(crossprod(M, Conj(x)), x)}  # taken from emulator

`checkerB` <- function(B){
  if(is.consistent.boost(B)){
    return(TRUE)
  } else {
    print(B)
    print(sol())
    print(eta())
    print(quad.form(eta(),B))
    print(quad.form(eta(),B) - eta())
    stop()
    return(FALSE)
  }
}



sol(1)         ; checker1(as.4vel(r3vel(10)))
sol(10)        ; checker1(as.4vel(r3vel(10)))
sol(100)       ; checker1(as.4vel(r3vel(10)))
sol(299792458) ; checker1(as.4vel(r3vel(10)))

sol(1)         ; checkerB(boost(r3vel(1)) %*% boost(r3vel(1)) %*% boost(r3vel(1)))
sol(10)        ; checkerB(boost(r3vel(1)) %*% boost(r3vel(1)) %*% boost(r3vel(1)))
sol(100)       ; checkerB(boost(r3vel(1)) %*% boost(r3vel(1)) %*% boost(r3vel(1)))
sol(299792458) ; checkerB(boost(r3vel(1)) %*% boost(r3vel(1)) %*% boost(r3vel(1)))




## reset speed of light:
sol(1)
