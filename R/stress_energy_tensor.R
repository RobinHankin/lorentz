`perfectfluid` <- function(rho,p=0,u=0){
  p <- p/sol()^2
  crossprod(as.4vel(u))*(rho+p) +  p*eta(downstairs=FALSE)
}

`dust` <- function(rho,u=0){perfectfluid(rho,p=0,u=u)}
`photongas` <- function(rho,u=0){perfectfluid(rho,p=rho/3,u=u)}

`transform_uu` <-  function(TT,B){ tensor(B,tensor(B,TT,2,2),2,2) }
`transform_ud` <-  function(TT,B){ tensor(B,tensor(B,TT,1,2),2,2) }
`transform_dd` <-  function(TT,B){ tensor(B,tensor(B,TT,1,2),1,2) }

`raise` <- function(TT){flob(emulator::quad.form(TT,eta(downstairs=FALSE)))}
`lower` <- function(TT){flob(emulator::quad.form(TT,eta(downstairs= TRUE)))}


`ptm` <- function(to_natural=TRUE, change_time=TRUE){ # passive transformation matrix
  ## inline comments refer to changing to/from SI
  if(to_natural){                # change from mks TO natural units
    if(change_time){             # unit of time changes 
      out <- c(sol(),1,1,1)      # one second = 3e8 meters
    } else {                     # unit of distance changes
      out <- c(1,rep(1/sol(),3)) # one meter = 3e-9 seconds
    }
  } else {                       # change FROM natural units units to mks
    if(change_time){             # unit of time changes
      out <- c(1/sol(),1,1,1)    # one meter = 3e-9 seconds
    } else {                     # unit of distance changes
      out <- c(1,rep(sol(),3))   # one second = 3e8 meters
    }
  }
  return(flob(diag(out)))
}
