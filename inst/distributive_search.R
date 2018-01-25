library(gyrogroup)
library(partitions)

u <- r3vel(1,0.4)
v <- r3vel(1,0.5)
w <- r3vel(1,0.6)
r <- 1.5

every_sign <- function(a1,a2,a3,a4,a5){
  c(
      all3(c(a1 , a2 , +gyr(+a3,+a4,+a5))),
      all3(c(a1 , a2 , +gyr(+a3,+a4,-a5))),
      all3(c(a1 , a2 , +gyr(+a3,-a4,+a5))),
      all3(c(a1 , a2 , +gyr(+a3,-a4,-a5))),
      all3(c(a1 , a2 , +gyr(-a3,+a4,+a5))),
      all3(c(a1 , a2 , +gyr(-a3,+a4,-a5))),
      all3(c(a1 , a2 , +gyr(-a3,-a4,+a5))),
      all3(c(a1 , a2 , +gyr(-a3,-a4,-a5))),
      all3(c(a1 , a2 , -gyr(+a3,+a4,+a5))),
      all3(c(a1 , a2 , -gyr(+a3,+a4,-a5))),
      all3(c(a1 , a2 , -gyr(+a3,-a4,+a5))),
      all3(c(a1 , a2 , -gyr(+a3,-a4,-a5))),
      all3(c(a1 , a2 , -gyr(-a3,+a4,+a5))),
      all3(c(a1 , a2 , -gyr(-a3,+a4,-a5))),
      all3(c(a1 , a2 , -gyr(-a3,-a4,+a5))),
      all3(c(a1 , a2 , -gyr(-a3,-a4,-a5)))
  )
}

possible <- function(u,v,r){
  out <-
    c(
        every_sign(r*u,r*v, u,v, u+v),
        every_sign(r*u,r*v, u,v, v+u),
        every_sign(r*u,r*v, v,u, u+v),
        every_sign(r*u,r*v, v,u, v+u),

        every_sign(r*u,r*v, u,v+u, u+v),
        every_sign(r*u,r*v, u,v+u, v+u),
        every_sign(r*u,r*v, u,u+v, u+v),
        every_sign(r*u,r*v, u,u+v, v+u),

        every_sign(r*u,r*v, u+v,v, u+v),
        every_sign(r*u,r*v, u+v,v, v+u),
        every_sign(r*u,r*v, v+u,u, u+v),
        every_sign(r*u,r*v, v+u,u, v+u),

        every_sign(r*u,r*v, u+v,v+u, u+v),
        every_sign(r*u,r*v, u+v,v+u, v+u),
        every_sign(r*u,r*v, v+u,u+v, u+v),
        every_sign(r*u,r*v, v+u,u+v, v+u),


        all3(c(r*u , r*v , r*gyr(+u,+v,u+v)))     ,
        all3(c(r*u , r*v , r*gyr(+u,+v,v+u)))     ,
        all3(c(r*u , r*v , r*gyr(+u,-v,u+v)))     ,
        all3(c(r*u , r*v , r*gyr(+u,-v,v+u)))     ,
        all3(c(r*u , r*v , r*gyr(-u,+v,u+v)))     ,
        all3(c(r*u , r*v , r*gyr(-u,+v,v+u)))     ,
        all3(c(r*u , r*v , r*gyr(-u,-v,u+v)))     ,
        all3(c(r*u , r*v , r*gyr(-u,-v,v+u)))     ,

        all3(c(r*u , r*v , r*gyr(+v,+u,u+v)))     ,
        all3(c(r*u , r*v , r*gyr(+v,+u,v+u)))     ,
        all3(c(r*u , r*v , r*gyr(+v,-u,u+v)))     ,
        all3(c(r*u , r*v , r*gyr(+v,-u,v+u)))     ,
        all3(c(r*u , r*v , r*gyr(-v,+u,u+v)))     ,
        all3(c(r*u , r*v , r*gyr(-v,+u,v+u)))     ,
        all3(c(r*u , r*v , r*gyr(-v,-u,u+v)))     ,
        all3(c(r*u , r*v , r*gyr(-v,-u,v+u)))     ,

        all3(c(r*u , r*v , -r*gyr(+u,+v,u+v)))     ,
        all3(c(r*u , r*v , -r*gyr(+u,+v,v+u)))     ,
        all3(c(r*u , r*v , -r*gyr(+u,-v,u+v)))     ,
        all3(c(r*u , r*v , -r*gyr(+u,-v,v+u)))     ,
        all3(c(r*u , r*v , -r*gyr(-u,+v,u+v)))     ,
        all3(c(r*u , r*v , -r*gyr(-u,+v,v+u)))     ,
        all3(c(r*u , r*v , -r*gyr(-u,-v,u+v)))     ,
        all3(c(r*u , r*v , -r*gyr(-u,-v,v+u)))     ,

        all3(c(r*u , r*v , -r*gyr(+v,+u,u+v)))     ,
        all3(c(r*u , r*v , -r*gyr(+v,+u,v+u)))     ,
        all3(c(r*u , r*v , -r*gyr(+v,-u,u+v)))     ,
        all3(c(r*u , r*v , -r*gyr(+v,-u,v+u)))     ,
        all3(c(r*u , r*v , -r*gyr(-v,+u,u+v)))     ,
        all3(c(r*u , r*v , -r*gyr(-v,+u,v+u)))     ,
        all3(c(r*u , r*v , -r*gyr(-v,-u,u+v)))     ,
        all3(c(r*u , r*v , -r*gyr(-v,-u,v+u)))     


    )

  return(out)

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


plot(1/prod3(r*(u+v) - possible(u,v,r)))
