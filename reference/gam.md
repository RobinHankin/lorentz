# Gamma correction

Lorentz gamma correction term in special relativity

## Usage

``` r
# S3 method for class '3vel'
speed(u)
# S3 method for class '4vel'
speed(u)
speedsquared(u)
gam(u)
gamm1(u)
# S3 method for class '3vel'
gam(u)
# S3 method for class '3cel'
gam(u)
# S3 method for class '4vel'
gam(u)
# S3 method for class '3vel'
gamm1(u)
# S3 method for class '4vel'
gamm1(u)
gam_ur(d)
```

## Arguments

- u:

  Speed: either a vector of speeds or a vector of three-velocities or
  four-velocities

- d:

  In function `gam_ur()`, deficit of speed; speed of light minus speed
  of object

## Details

Function `speed(u)` returns the speed of a `3vel` object or `4vel`
object.

Function `gam(u)` returns the Lorentz factor
\$\$\frac{1}{\sqrt{1-\mathbf{u}\cdot\mathbf{u}/c^2}}\$\$

Function `gamm1(u)` returns the Lorentz factor minus 1, useful for slow
speeds when larger accuracy is needed (much like
[`expm1()`](https://rdrr.io/r/base/Log.html)); to see the R idiom, type
“`gamm1.3vel`” at the commandline. Function `gamm1()` is intended to
work with `3vel` objects or speeds. The function will take a 4-velocity,
but this is not recommended as accuracy is lost (all it does is return
the time component of the 4-velocity minus 1).

Function `gam_ur()` is used for the ultrarelativistic case where speeds
are very close to the speed of light (the function is named for “gamma,
ultrarelativistic”). Its argument `d` is the deficit, that is, \\c-v\\
where \\v\\ is the speed of the transformation. Algebraically,
`gam_ur(c-v) == gam(v)`, but if `d` is small compared to `c` the result
is more accurate.

Function `speedsquared(u)` returns the square of the speed of a `3vel`
object. Use this to avoid taking a needless square root.

## Author

Robin K. S. Hankin

## Examples

``` r
gam(seq(from=0,by=0.1,len=10))
#>  [1] 1.000000 1.005038 1.020621 1.048285 1.091089 1.154701 1.250000 1.400280
#>  [9] 1.666667 2.294157
gam(r3vel(6,0.7))
#> [1] 1.40028 1.40028 1.40028 1.40028 1.40028 1.40028


x <- as.3vel(c(0.1,0.4,0.5))
speed(x)
#> [1] 0.6480741

gam(speed(x))  # works, but slow and inaccurate
#> [1] 1.313064
gam(x)         # recommended: avoids needless coercion
#> [1] 1.313064



## Use SI units and deal with terrestrial speeds.  Use gamm1() for this.
sol(299792458)
#> [1] 299792458
sound <- 343 # speed of sound in SI
gam(sound)
#> [1] 1
gam(sound)-1  
#> [1] 6.545875e-13
gamm1(sound)   # gamm1() gives much higher precision
#> [1] 6.545108e-13

snail <- as.3vel(c(0.00275,0,0)) # even the world's fastest snail...
gamm1(snail)                     # ...has only a small relativistic correction
#> [1] 4.207208e-23


## For the ultrarelativistic case of speeds very close to the speed of
## light, use gam_ur():

sol(1)           # revert to relativistic units
#> [1] 1

gam(0.99) - gam_ur(0.01) # zero to numerical accuracy
#> [1] -4.440892e-15

omgp <- 4.9e-24  # speed deficit of the Oh-My-God particle
gam(1-omgp)      # numeric overflow
#> [1] Inf
gam_ur(omgp)     # large but finite
#> [1] 319438282500
```
