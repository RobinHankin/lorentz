`perfectfluid` <- function(rho,p=0,u=0){
  p <- p/sol()^2
  crossprod(as.4vel(u))*(rho+p) +  p*eta(downstairs=FALSE)
}

`dust` <- function(rho,u=0){perfectfluid(rho,p=0,u=u)}
`photongas` <- function(rho,u=0){perfectfluid(rho,p=rho/3,u=u)}

`transform_uu` <-  function(TT,B){ tensor(B,tensor(B,TT,2,2),2,2) }
`transform_ud` <-  function(TT,B){ tensor(B,tensor(B,TT,1,2),2,2) }
`transform_dd` <-  function(TT,B){ tensor(B,tensor(B,TT,1,2),1,2) }
