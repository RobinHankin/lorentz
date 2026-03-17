# Arithmetic Ops Group Methods for 3vel objects

Arithmetic operations for three-velocities

## Usage

``` r
# S3 method for class '3vel'
Ops(e1, e2)
# S3 method for class '4vel'
Ops(e1, e2)
massage3(u,v)
neg3(u)
prod3(u,v=u)
add3(u,v)
dot3(v,r)
```

## Arguments

- e1,e2,u,v:

  Objects of class “`3vel`”, three-velocities

- r:

  Scalar value for circle-dot multiplication

## Details

The function `Ops.3vel()` passes unary and binary arithmetic operators
“`+`”, “`-`” and “`*`” to the appropriate specialist function.

The most interesting operators are “`+`” and “`*`”, which are passed to
`add3()` and `dot3()` respectively. These are defined, following Ungar,
as:

\$\$ \mathbf{u}+\mathbf{v} = \frac{1}{1+\mathbf{u}\cdot\mathbf{b}/c^2}
\left\\ \mathbf{u} + \frac{1}{\gamma\_\mathbf{u}}\mathbf{v} +
\frac{1}{c^2}\frac{\gamma\_\mathbf{u}}{1+\gamma\_\mathbf{u}}
\left(\mathbf{u}\cdot\mathbf{v}\right)\mathbf{u} \right\\ \$\$

and

\$\$ r\odot\mathbf{v} = c\tanh\left(
r\tanh^{-1}\frac{\left\|\left\|\mathbf{v}\right\|\right\|}{c}
\right)\frac{\mathbf{v}}{\left\|\left\|\mathbf{v}\right\|\right\|} \$\$

where \\\mathbf{u}\\ and \\\mathbf{v}\\ are three-vectors and \\r\\ a
scalar. Function `dot3()` has special dispensation for zero velocity and
does not treat `NA` entries entirely consistently.

Arithmetic operations, executed via `Ops.4vel()`, are not defined on
four-velocities.

The package is designed so that natural R idiom may be used for three
velocity addition, see the examples section.

## Value

Returns an object of class `3vel`, except for `prod3()` which returns a
numeric vector.

## Examples

``` r
u <- as.3vel(c(-0.7, 0.1,-0.1))
v <- as.3vel(c( 0.1, 0.2, 0.3))
w <- as.3vel(c( 0.5, 0.2,-0.3))

x <- r3vel(10)   # random three velocities
y <- r3vel(10)   # random three velocities


u+v   # add3(u,v)
#> A vector of three-velocities (speed of light = 1)
#>              x         y         z
#> [1,] -0.648977 0.2557545 0.1246803
u-v   # add3(u,neg3(v))
#> A vector of three-velocities (speed of light = 1)
#>               x           y          z
#> [1,] -0.7434641 -0.03267974 -0.2913943

-v    # neg3(v)
#> A vector of three-velocities (speed of light = 1)
#>         x    y    z
#> [1,] -0.1 -0.2 -0.3

gyr(u,v,w)
#> A vector of three-velocities (speed of light = 1)
#>              x         y          z
#> [1,] 0.5134003 0.2390541 -0.2434611

## package is vectorized:


u+x
#> A vector of three-velocities (speed of light = 1)
#>                x           y           z
#>  [1,] -0.7841682  0.02734861 -0.50404915
#>  [2,] -0.7036563 -0.30641050 -0.36843047
#>  [3,] -0.8587346 -0.08670915 -0.21410624
#>  [4,] -0.9480249 -0.12030137  0.02246383
#>  [5,] -0.3781309  0.71645428 -0.14413320
#>  [6,] -0.9691780  0.12138302 -0.13643205
#>  [7,] -0.8994853  0.12338245  0.03639300
#>  [8,] -0.7238049  0.38382395  0.47428504
#>  [9,] -0.7111436  0.29068074 -0.26074149
#> [10,] -0.4477571 -0.31206541 -0.49728634
x+y
#> A vector of three-velocities (speed of light = 1)
#>                 x          y           z
#>  [1,] -0.09600868 -0.7179919 -0.36065284
#>  [2,]  0.28002674 -0.7598638 -0.51931521
#>  [3,] -0.33840429  0.3279208 -0.15803202
#>  [4,] -0.56337171 -0.7201825  0.09843981
#>  [5,]  0.84478030  0.2635985  0.07562038
#>  [6,] -0.85828999  0.3694929 -0.34551186
#>  [7,] -0.34587079 -0.1200680  0.76226856
#>  [8,] -0.14321273  0.1191482  0.88090820
#>  [9,] -0.58013943  0.7018523 -0.17781538
#> [10,]  0.46907486 -0.5528338 -0.46070785

f <- gyrfun(u,v)
g <- gyrfun(v,u)

f(g(x)) - x    # should be zero by eqn10
#> A vector of three-velocities (speed of light = 1)
#>                   x             y            z
#>  [1,] -5.853023e-16  2.237921e-16 1.377182e-16
#>  [2,]  4.847343e-17 -1.551150e-16 1.034100e-16
#>  [3,]  4.440762e-16  3.633351e-16 1.412970e-16
#>  [4,] -1.130274e-15  1.614678e-16 1.614678e-16
#>  [5,] -6.654321e-16 -4.436214e-16 8.317901e-17
#>  [6,]  1.449847e-15  2.718464e-16 1.812309e-16
#>  [7,]  2.974155e-16  9.559783e-17 1.805737e-16
#>  [8,] -8.776910e-16  1.101416e-15 8.260621e-16
#>  [9,] -3.322553e-16 -1.200673e-16 4.140253e-18
#> [10,] -3.334777e-16  4.446369e-16 4.446369e-16
g(f(x)) - x
#> A vector of three-velocities (speed of light = 1)
#>                   x             y             z
#>  [1,] -5.508728e-16  1.032886e-16 -4.131546e-16
#>  [2,] -5.606760e-16 -2.585250e-16 -1.551150e-16
#>  [3,] -8.275965e-16  8.074112e-17 -9.083376e-17
#>  [4,] -1.130274e-15  0.000000e+00  4.036694e-16
#>  [5,] -7.393690e-16  4.436214e-16  7.393690e-17
#>  [6,]  5.074466e-15 -2.265387e-16  1.178001e-15
#>  [7,] -8.922464e-16  2.443056e-16  5.098551e-16
#>  [8,] -8.260621e-16  1.239093e-15  1.101416e-15
#>  [9,]  1.749257e-16 -5.382329e-17  1.242076e-16
#> [10,]  0.000000e+00  1.111592e-16 -5.557961e-17


(u+v) - f(v+u)                     # zero by eqn 10
#> A vector of three-velocities (speed of light = 1)
#>                  x            y            z
#> [1,] -6.132309e-16 1.951189e-16 1.672448e-16
(u+(v+w)) - ((u+v)+f(w))           # zero by eqn 11
#> A vector of three-velocities (speed of light = 1)
#>                  x y             z
#> [1,] -5.469679e-17 0 -5.469679e-18
((u+v)+w) - (u+(v+g(w)))           # zero by eqn 11
#> A vector of three-velocities (speed of light = 1)
#>                 x            y            z
#> [1,] 3.951349e-16 -2.99021e-16 1.975675e-16


## NB, R idiom is unambiguous.  But always always ALWAYS use brackets.

## Ice report in lat 42.n to 41.25n Long 49w to long 50.30w saw much
## heavy pack ice and great number large icebergs also field
## ice.  Weather good clear

## -u+v == (-u) + v == neg3(u) + v == add3(neg3(u),v)

## u+v+w == (u+v)+w == add3(add3(u,v),w)
```
