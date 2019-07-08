## This is a systematic attempt to find a distributive law for
## three-velocities: r*(u+v) = <some function of u,v,r>.

## Here, u and v are three-velocities and r is a real number.

## In this script, function possible() defines a large number of
## possible combinations of u, v, and r.  These are calculated at the
## end of the script and the results compared with the exact value.
## If there is an exact expression, this will have zero badness.

## I have tried to enumerate the various combinations of u,v,r.  Signs
## are permuted in every_sign() and all3() and all3brack().  Just
## execute the script and if one of the many combinations (688128, as
## of 26/1/2018) represents the RHS of a putative distributive law,
## the corresponding element of variable 'badness', defined at the end
## of this script, will be zero.  I have not found one yet.

## Function possible() returns a long vector of possible expressions
## that might form the RHS for a distributive law.  The basic idea is
## that r*(u+v) = ru+rv + <some correction> but because of
## noncommutativity and nonassociativity, this gets complicated.

## Function possible() returns a vector that includes things like

## ru  + (rv  - r*gyr[ru, v,u+v])
## ru  + (rv  - r*gyr[ru, v,v+u])
## rv  + (ru  - r*gyr[su, v,u+v])
## (ru + rv)  + r*gyr[ru, v,u])
## (ru + rv)  + r*gyr[ru, v,v])
## (ru + rv)  + r*gyr[ru, v,v])
## (ru + rv)  + r*gyr[ru,-v,v])
## (ru + rv)  +   gyr[ru,-v,v])
## (ru + rv)  + s*gyr[ru,-v,v])

## etc etc etc.  Here, r is a scalar, s=1/r, and u,v are
## three-velocities.  Note the variety of orders (three-velocity
## addition is not commutative), different bracketing (three-velocity
## addition is not associative), and use of r or 1/r in differnt
## places in the formula.


library("lorentz")
library("partitions")

u <- r3vel(1,0.4)
v <- r3vel(1,0.5)
r <- 2

`possible` <- function(u,v,r){ 
 # In function possible(), u,v are three-velocities and r is a real
 # number.  Function possible() returns a vector of 64*10752=688128
 # three-velocities [the '64' is from combinations of r, 1/r as in jj
 # below; the 10752 is from function f(), defined inside possible()]

  f <- function(r1,r2,r3){  
    ##  In function f(), r1,r2,r3 are real numbers; function f() has
    ##  4*14 = 56 lines, so returns 56*192=10752 three-velocities.

    ## NB: Inside f(), r,u,v come from possible()'s scope
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
  ## jj has 4^3=64 rows
  out <- u
  for(i in seq_len(nrow(jj))){
    cat(paste(i," / ",nrow(jj),"\n",sep=""))
    out <- c(out,f(jj[i,1],jj[i,2],jj[i,3]))
  } 
  return(out)  # out has 64*18432=1179648 elements
}

`every_sign` <- function(a1,a2,a3,a4,a5,r){
 # Function every_sign() has 16 lines; given 5 three-velocities and a
 # scalar, function every_sign() returns a vector of 16*12=192
 # three-velocities [the 12 is from all3()]
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

`all3brack` <- function(x){  # If [abc] = c(x[1],x[2],x[3]), function
                             # all3brack() returns a+(b+c) and (a+b)+c
  c(
      x[1]+(x[2]+x[3])   ,
      (x[1]+x[2])+x[3]
  )
}

`all4brack` <- function(x){
  ## Returns the 5 different ways to bracket 4 objects.  Function
  ## all4vbrack() not currentluy used in this script.
  c(
  (x[1]+x[2])+(x[3]+x[4])   ,
  ((x[1]+x[2])+x[3])+x[4]   ,
  (x[1]+(x[2]+x[3]))+x[4]   ,
  x[1]+((x[2]+x[3])+x[4])   ,
  x[1]+(x[2]+(x[3]+x[4]))
  )
}

`all4` <- function(x){  
 ## Every possible way of combining 4 three-velocities.  Function
 ## all4() is not currently used in this script
  stopifnot(length(x)==4)
  out <- threevel(0)
  jj <- perms(4)
  for(i in seq_len(ncol(jj))){
    out <- c(out,all4brack(x[jj[,i]]))
  }
  return(out)
}

`all3` <- function(x){   # every possible way of combining 3
                         # three-velocities.  Function; if x=c(a,b,c)
                         # with a,b,c three-velocities then all3()
                         # returns a vector of length 2*3!=12

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
