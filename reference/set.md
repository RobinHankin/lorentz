# The energy-momentum tensor

Various functionality to deal with the stress-energy tensor in special
relativity.

## Usage

``` r
perfectfluid(rho, p, u=0)
dust(rho, u=0)
photongas(rho, u=0)
transform_dd(TT, B)
transform_ud(TT, B)
transform_uu(TT, B)
raise(TT)
lower(TT)
```

## Arguments

- TT:

  A second-rank tensor with indices either downstairs-downstairs,
  downstairs-upstairs, or upstairs-upstairs

- B:

  A boost matrix

- rho,p,u:

  Density, pressure, and four-velocity of the dust

## Details

Function `perfectfluid()` returns the stress-energy tensor, with two
upstairs indices, for a perfect fluid with the conditions specified. No
checking for physical reasonableness (eg the weak energy condition) is
performed: caveat emptor!

Function `dust()` is a (trivial) function that returns the stress-energy
tensor of a zero-pressure perfect fluid (that is, dust). Function
`photongas()` returns the stress-energy tensor of a photon gas. They are
here for discoverability reasons; both are special cases of a perfect
fluid.

Functions `transform_dd()` et seq transform a second-rank tensor using
the Lorentz transformation. The letters “u” or “d” denote the indices of
the tensor being upstairs (contravariant) or downstairs (covariant). The
stress-energy tensor is usually written with two upstairs indices, so
use `transform_uu()` to transform it.

Function `lower()` lowers both indices of a tensor with two upstairs
indices. Function `raise()` raises two downstairs indices. These two
functions have identical R idiom but do not return identical values if
\\c\neq 1\\.

## Author

Robin K. S. Hankin

## Examples

``` r

perfectfluid(10, 1)
#>    t x y z
#> t 10 0 0 0
#> x  0 1 0 0
#> y  0 0 1 0
#> z  0 0 0 1

u <- as.3vel(c(0.4, 0.4, 0.2))

## In the following, LHS is stationary dust and RHS is dust moving at
## velocity 'u', but transformed to a frame also moving at velocity 'u':

LHS <- dust(1)
RHS <- transform_uu(dust(1,u), boost(u))
max(abs(LHS-RHS))  # should be small
#> [1] 6.938894e-17


## In the following, negative sign needed because active/passive
## difference:

LHS <- dust(1, u)
RHS <- transform_uu(dust(1), boost(-u))
max(abs(LHS-RHS))  # should be small
#> [1] 0

## Now test behaviour when  c!=1:


sol(299792458)
#> [1] 299792458
perfectfluid(1.225, 101325) # air at STP
#>       t            x            y            z
#> t 1.225 0.000000e+00 0.000000e+00 0.000000e+00
#> x 0.000 1.127393e-12 0.000000e+00 0.000000e+00
#> y 0.000 0.000000e+00 1.127393e-12 0.000000e+00
#> z 0.000 0.000000e+00 0.000000e+00 1.127393e-12

LHS <- transform_uu(perfectfluid(1.225,101325), boost(as.3vel(c(1000,0,0))))
RHS <- perfectfluid(1.225, 101325) 
LHS-RHS  # should be small
#>               t       x y z
#> t  1.362999e-11   -1225 0 0
#> x -1.225000e+03 1225000 0 0
#> y  0.000000e+00       0 0 0
#> z  0.000000e+00       0 0 0

sol(10)
#> [1] 10
u <- as.3vel(4:6)
LHS <- photongas(1, u)
RHS <- transform_uu(photongas(1), boost(-u))
LHS-RHS # should be small
#>               t             x             y             z
#> t -8.881784e-16 -3.552714e-15  3.552714e-15  0.000000e+00
#> x -3.552714e-15  0.000000e+00  1.421085e-14  0.000000e+00
#> y  3.552714e-15  1.421085e-14  1.421085e-14 -2.842171e-14
#> z  0.000000e+00  0.000000e+00 -2.842171e-14 -2.842171e-14



B1 <- boost(r3vel(1)) %*% boost(r3vel(1))
B2 <- boost(r3vel(1)) %*% boost(r3vel(1))
LHS <- transform_uu(transform_uu(dust(1), B1), B2)
RHS <- transform_uu(dust(1),B2 %*% B1)   # note order
LHS-RHS  # should be small
#>              t             x             y            z
#> t 2.273737e-13  1.818989e-12  4.547474e-13 9.094947e-13
#> x 1.818989e-12 -1.455192e-11 -7.275958e-12 0.000000e+00
#> y 1.364242e-12  0.000000e+00  0.000000e+00 0.000000e+00
#> z 2.273737e-12  0.000000e+00  0.000000e+00 3.637979e-12


## remember to re-set c:
sol(1)
#> [1] 1
```
