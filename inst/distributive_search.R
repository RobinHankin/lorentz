## This is a systematic attempt to find a distributive law for
## three-velocities: r*(u+v) = <some function of u,v,r>.

## I have tried to enumerate the various combinations; signs are
## permuted in every_sign() and all3() and all3brack().  Just execute
## the script and if one of the many combinations (196609, as of
## 26/1/2018) represents the RHS of a putative distributive law, the
## result will be zero.  I have not found one yet.



library(gyrogroup)
library(partitions)

u <- r3vel(1,0.4)
v <- r3vel(1,0.5)
w <- r3vel(1,0.6)
r <- 1.5

`possible` <- function(u,v,r){
  f <- function(r1,r2,r3){
    c(
        every_sign(r*u,r*v, r1*u     ,r2*v  , u+v,r3),
        every_sign(r*u,r*v, r1*u     ,r2*v  , v+u,r3),
        every_sign(r*u,r*v, r1*v     ,r2*u  , u+v,r3),
        every_sign(r*u,r*v, r1*v     ,r2*u  , v+u,r3),

        every_sign(r*u,r*v, r1*u     ,r2*v  , u-v,r3),
        every_sign(r*u,r*v, r1*u     ,r2*v  , v-u,r3),
        every_sign(r*u,r*v, r1*v     ,r2*u  , u-v,r3),
        every_sign(r*u,r*v, r1*v     ,r2*u  , v-u,r3),
        
        every_sign(r*u,r*v, r1*u     ,r2*v+u, u+v,r3),
        every_sign(r*u,r*v, r1*u     ,r2*v+u, v+u,r3),
        every_sign(r*u,r*v, r1*u     ,r2*u+v, u+v,r3),
        every_sign(r*u,r*v, r1*u     ,r2*u+v, v+u,r3),

        every_sign(r*u,r*v, r1*u     ,r2*v-u, u+v,r3),
        every_sign(r*u,r*v, r1*u     ,r2*v-u, v+u,r3),
        every_sign(r*u,r*v, r1*u     ,r2*u-v, u+v,r3),
        every_sign(r*u,r*v, r1*u     ,r2*u-v, v+u,r3),

        every_sign(r*u,r*v, r1*u     ,r2*v+u, u-v,r3),
        every_sign(r*u,r*v, r1*u     ,r2*v+u, v-u,r3),
        every_sign(r*u,r*v, r1*u     ,r2*u+v, u-v,r3),
        every_sign(r*u,r*v, r1*u     ,r2*u+v, v-u,r3),
        
        every_sign(r*u,r*v, r1*u     ,r2*v-u, u-v,r3),
        every_sign(r*u,r*v, r1*u     ,r2*v-u, v-u,r3),
        every_sign(r*u,r*v, r1*u     ,r2*u-v, u-v,r3),
        every_sign(r*u,r*v, r1*u     ,r2*u-v, v-u,r3),

        
        every_sign(r*u,r*v, r1*u     ,v+r2*u, u+v,r3),
        every_sign(r*u,r*v, r1*u     ,v+r2*u, v+u,r3),
        every_sign(r*u,r*v, r1*u     ,u+r2*v, u+v,r3),
        every_sign(r*u,r*v, r1*u     ,u+r2*v, v+u,r3),
        
        every_sign(r*u,r*v, r1*u     ,v+r2*u, u-v,r3),
        every_sign(r*u,r*v, r1*u     ,v+r2*u, v-u,r3),
        every_sign(r*u,r*v, r1*u     ,u+r2*v, u-v,r3),
        every_sign(r*u,r*v, r1*u     ,u+r2*v, v-u,r3),
        
        every_sign(r*u,r*v, r1*u     ,v-r2*u, u+v,r3),
        every_sign(r*u,r*v, r1*u     ,v-r2*u, v+u,r3),
        every_sign(r*u,r*v, r1*u     ,u-r2*v, u+v,r3),
        every_sign(r*u,r*v, r1*u     ,u-r2*v, v+u,r3),
        
        every_sign(r*u,r*v, r1*u     ,v-r2*u, u-v,r3),
        every_sign(r*u,r*v, r1*u     ,v-r2*u, v-u,r3),
        every_sign(r*u,r*v, r1*u     ,u-r2*v, u-v,r3),
        every_sign(r*u,r*v, r1*u     ,u-r2*v, v-u,r3),
        
        every_sign(r*u,r*v, r1*u+r2*v,v     , u+v,r3),
        every_sign(r*u,r*v, r1*u+r2*v,v     , v+u,r3),
        every_sign(r*u,r*v, r1*v+r2*u,u     , u+v,r3),
        every_sign(r*u,r*v, r1*v+r2*u,u     , v+u,r3),
        
        every_sign(r*u,r*v, r1*u+r2*v,v     , u-v,r3),
        every_sign(r*u,r*v, r1*u+r2*v,v     , v-u,r3),
        every_sign(r*u,r*v, r1*v+r2*u,u     , u-v,r3),
        every_sign(r*u,r*v, r1*v+r2*u,u     , v-u,r3),
        
        every_sign(r*u,r*v, r1*u-r2*v,v     , u+v,r3),
        every_sign(r*u,r*v, r1*u-r2*v,v     , v+u,r3),
        every_sign(r*u,r*v, r1*v-r2*u,u     , u+v,r3),
        every_sign(r*u,r*v, r1*v-r2*u,u     , v+u,r3),
        
        every_sign(r*u,r*v, r1*u-r2*v,v     , u-v,r3),
        every_sign(r*u,r*v, r1*u-r2*v,v     , v-u,r3),
        every_sign(r*u,r*v, r1*v-r2*u,u     , u-v,r3),
        every_sign(r*u,r*v, r1*v-r2*u,u     , v-u,r3)

    )
  }

  jj <- c(r,1,1/r,0)
  jj <- as.matrix(expand.grid(jj,jj,jj))
  out <- u
  for(i in seq_len(nrow(jj))){
    cat(paste(i," / ",nrow(jj),"\n",sep=""))
    out <- c(out,f(jj[i,1],jj[i,2],jj[i,3]))
  }
  return(out)
}

`every_sign` <- function(a1,a2,a3,a4,a5,r){
  c(
      all3(c(a1 , a2 , +r*gyr(+a3,+a4,+a5))),
      all3(c(a1 , a2 , +r*gyr(+a3,+a4,-a5))),
      all3(c(a1 , a2 , +r*gyr(+a3,-a4,+a5))),
      all3(c(a1 , a2 , +r*gyr(+a3,-a4,-a5))),
      all3(c(a1 , a2 , +r*gyr(-a3,+a4,+a5))),
      all3(c(a1 , a2 , +r*gyr(-a3,+a4,-a5))),
      all3(c(a1 , a2 , +r*gyr(-a3,-a4,+a5))),
      all3(c(a1 , a2 , +r*gyr(-a3,-a4,-a5))),
      all3(c(a1 , a2 , -r*gyr(+a3,+a4,+a5))),
      all3(c(a1 , a2 , -r*gyr(+a3,+a4,-a5))),
      all3(c(a1 , a2 , -r*gyr(+a3,-a4,+a5))),
      all3(c(a1 , a2 , -r*gyr(+a3,-a4,-a5))),
      all3(c(a1 , a2 , -r*gyr(-a3,+a4,+a5))),
      all3(c(a1 , a2 , -r*gyr(-a3,+a4,-a5))),
      all3(c(a1 , a2 , -r*gyr(-a3,-a4,+a5))),
      all3(c(a1 , a2 , -r*gyr(-a3,-a4,-a5)))
  )
}


`all3brack` <- function(x){  # x = c(x[1],x[2],x[3])
  c(
      x[1]+(x[2]+x[3])   ,
      (x[1]+x[2])+x[3]
  )
}

`all4brack` <- function(x){
  c(
  (x[1]+x[2])+(x[3]+x[4])   ,
  ((x[1]+x[2])+x[3])+x[4]   ,
  (x[1]+(x[2]+x[3]))+x[4]   ,
  x[1]+((x[2]+x[3])+x[4])   ,
  x[1]+(x[2]+(x[3]+x[4]))
  )
}

`all4` <- function(x){   # every possible way of combining 4 threevelocities
  stopifnot(length(x)==4)
  out <- threevel(0)
  jj <- perms(4)
  for(i in seq_len(ncol(jj))){
    out <- c(out,all4brack(x[jj[,i]]))
  }
  return(out)
}

`all3` <- function(x){   # every possible way of combining 3 threevelocities
  stopifnot(length(x)==3)
  out <- threevel(0)
  jj <- perms(3)
  for(i in seq_len(ncol(jj))){
    out <- c(out,all3brack(x[jj[,i]]))
  }
  return(out)
}


badness <- prod3(r*(u+v) - possible(u,v,r))  # badness == 0 for perfect law
print(min(badness))
