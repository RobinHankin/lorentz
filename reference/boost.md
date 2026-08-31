# Lorentz transformations

Lorentz transformations: boosts and rotations

## Usage

``` r
boost(u=0)
rot(u, v, space=TRUE)
is.consistent.boost(L, give=FALSE, TOL=1e-10)
is.consistent.boost.galilean(L, give=FALSE, TOL=1e-10)
pureboost(L, include_sol=TRUE)
orthog(L)
pureboost.galilean(L, tidy=TRUE)
orthog.galilean(L)
```

## Arguments

- u, v:

  Three-velocities, coerced to class `3vel`. In function `boost()`, if
  `u` takes the special default value `0`, this is interpreted as zero
  three velocity

- L:

  Lorentz transformation expressed as a \\4\times 4\\ matrix

- TOL:

  Numerical tolerance

- give:

  Boolean with `TRUE` meaning to return the transformed metric tensor
  (which should be the flat-space
  [`eta()`](https://robinhankin.github.io/lorentz/reference/sol.md); qv)
  and default `FALSE` meaning to return whether the matrix is a
  consistent boost or not

- space:

  Boolean, with default `TRUE` meaning to return just the spatial
  component of the rotation matrix and `FALSE` meaning to return the
  full \\4\times 4\\ matrix transformation

- tidy:

  In `pureboost.galilean()`, Boolean with default `TRUE` meaning to
  return a “tidy” boost matrix with spatial components forced to be a
  \\3\times 3\\ identity matrix

- include_sol:

  In function `pureboost()`, Boolean with default `TRUE` meaning to
  correctly account for the speed of light, and `FALSE` meaning to
  assume \\c=1\\. See details

## Value

Function `boost()` returns a \\4\times 4\\ matrix; function `rot()`
returns an orthogonal matrix.

## Details

Arguments `u, v` are coerced to three-velocities.

A rotation-free Lorentz transformation is known as a boost (sometimes a
pure boost), here expressed in matrix form. Pure boost matrices are
symmetric if \\c=1\\. Function `boost(u)` returns a \\4\times 4\\ matrix
giving the Lorentz transformation of an arbitrary three-velocity `u`.

Boosts can be successively applied with regular matrix multiplication.
However, composing two successive pure boosts does not in general return
a pure boost matrix: the product is not symmetric in general. Also note
that boost matrices do not commute. The resulting matrix product
represents a Lorentz transformation.

It is possible to decompose a Lorentz transformation \\L\\ into a pure
boost and a spatial rotation. Thus \\L=OP\\ where \\O\\ is an orthogonal
matrix and \\P\\ a pure boost matrix; these are returned by functions
`orthog()` and `pureboost()` respectively. If the speed of light is not
equal to 1, the functions still work but can be confusing.

Functions `pureboost.galilean()` and `orthog.galilean()` are the
Newtonian equivalents of `pureboost()` and `orthog()`, intended to be
used when the speed of light is infinite (which causes problems for the
relativistic functions).

As noted above, the composition of two pure Lorentz boosts is not
necessarily pure. If we have two successive boosts corresponding to
\\u\\ and \\v\\, then the composed boost may be decomposed into a pure
boost of `boost(u+v)` and a rotation of `rot(u,v)`.

The reason argument `include_sol` exists is that function `orthog()`
needs to call `pureboost()` in an environment where we pretend that
\\c=1\\.

## References

- Ungar 2006. “Thomas precession: a kinematic effect...”. European
  Journal of Physics, 27:L17-L20

- Sbitneva 2001. “Nonassociative geometry of special relativity”.
  International Journal of Theoretical Physics, volume 40, number 1,
  pages 359–362

- Wikipedia contributors 2018. “Wigner rotation”, Wikipedia, The Free
  Encyclopedia.
  <https://en.wikipedia.org/w/index.php?title=Wigner_rotation&oldid=838661305>.
  Online; accessed 23 August 2018

## Author

Robin K. S. Hankin

## Note

Function `rot()` uses
[`crossprod()`](https://rdrr.io/r/base/crossprod.html) for efficiency
reasons but is algebraically equivalent to

`boost(-u-v) %*% boost(u) %*% boost(v)`.

## Examples

``` r
boost(as.3vel(c(0.4,-0.2,0.1)))
#>            t           x           y           z
#> t  1.1250879 -0.45003516  0.22501758 -0.11250879
#> x -0.4500352  1.09530507 -0.04765253  0.02382627
#> y  0.2250176 -0.04765253  1.02382627 -0.01191313
#> z -0.1125088  0.02382627 -0.01191313  1.00595657

u <- r3vel(1)
v <- r3vel(1)
w <- r3vel(1)

boost(u) - solve(boost(-u))  # should be zero
#>               t             x             y            z
#> t  0.000000e+00 -1.110223e-16  1.110223e-16 0.000000e+00
#> x -5.551115e-17 -4.440892e-16 -2.081668e-17 3.469447e-17
#> y  0.000000e+00 -2.775558e-17  2.220446e-16 2.775558e-17
#> z  0.000000e+00  3.469447e-17  0.000000e+00 0.000000e+00

boost(u) %*% boost(v)   # not a pure boost (not symmetrical)
#>            t          x           y          z
#> t  1.9785365  0.6997728  0.09466761 -1.5543367
#> x  0.6577317  1.1507970 -0.06296776 -0.3229743
#> y  0.4431298  0.1902924  0.99271578 -0.4179333
#> z -1.5118305 -0.3593565  0.13968679  1.7711527
boost(u+v)  # not the same!
#>            t           x           y          z
#> t  1.9785365  0.65773173  0.44312981 -1.5118305
#> x  0.6577317  1.14524282  0.09785361 -0.3338482
#> y  0.4431298  0.09785361  1.06592635 -0.2249216
#> z -1.5118305 -0.33384815 -0.22492159  1.7673673
boost(v+u)  # also not the same!
#>             t           x           y           z
#> t  1.97853648  0.69977283  0.09466761 -1.55433672
#> x  0.69977283  1.16440357  0.02224106 -0.36517351
#> y  0.09466761  0.02224106  1.00300885 -0.04940189
#> z -1.55433672 -0.36517351 -0.04940189  1.81112407

u+v  # returns a three-velocity
#> A vector of three-velocities (speed of light = 1)
#>               x          y         z
#> [1,] -0.3324335 -0.2239685 0.7641156


boost(u) %*% boost(v) %*% boost(w)  # associative, no brackets needed
#>            t         x           y          z
#> t  2.4816250  1.843221 -0.04708488 -1.3261908
#> x  1.3585858  1.667719 -0.12705019 -0.2198357
#> y  0.5002695  0.432151  0.96273274 -0.3696766
#> z -1.7499822 -1.195585  0.24335345  1.6043047
boost(u+(v+w))  # not the same!
#>            t          x          y          z
#> t  2.4816250  1.3585858  0.5002695 -1.7499822
#> x  1.3585858  1.5301419  0.1952132 -0.6828711
#> y  0.5002695  0.1952132  1.0718830 -0.2514523
#> z -1.7499822 -0.6828711 -0.2514523  1.8796001
boost((u+v)+w)  # also not the same!
#>            t          x          y          z
#> t  2.4393350  1.3433462  0.4655575 -1.7114416
#> x  1.3433462  1.5246884  0.1818389 -0.6684602
#> y  0.4655575  0.1818389  1.0630191 -0.2316653
#> z -1.7114416 -0.6684602 -0.2316653  1.8516275


rot(u,v)
#>              x           y           z
#> x  0.996270498 -0.08387262  0.02026027
#> y  0.086184168  0.97863167 -0.18668783
#> z -0.004169345  0.18773769  0.98221035
rot(v,u)    # transpose (=inverse) of rot(u,v)
#>             x           y            z
#> x  0.99627050  0.08618417 -0.004169345
#> y -0.08387262  0.97863167  0.187737693
#> z  0.02026027 -0.18668783  0.982210352


rot(u,v,FALSE) %*% boost(v) %*% boost(u)
#>            t           x           y          z
#> t  1.9785365  0.65773173  0.44312981 -1.5118305
#> x  0.6577317  1.14524282  0.09785361 -0.3338482
#> y  0.4431298  0.09785361  1.06592635 -0.2249216
#> z -1.5118305 -0.33384815 -0.22492159  1.7673673
boost(u+v)     # should be the same.
#>            t           x           y          z
#> t  1.9785365  0.65773173  0.44312981 -1.5118305
#> x  0.6577317  1.14524282  0.09785361 -0.3338482
#> y  0.4431298  0.09785361  1.06592635 -0.2249216
#> z -1.5118305 -0.33384815 -0.22492159  1.7673673


orthog(boost(u) %*% boost(v)) - rot(u,v,FALSE)  # zero to numerical precision
#>               t             x             y             z
#> t -1.998401e-15  1.554635e-15  3.033290e-16 -1.210444e-15
#> x  6.478730e-15 -3.552714e-15 -4.440892e-16  5.100087e-15
#> y  1.580679e-15 -1.054712e-15 -6.661338e-16  9.714451e-16
#> z -3.201405e-15  2.118965e-15  2.775558e-17 -2.442491e-15
pureboost(boost(v) %*% boost(u)) - boost(u+v)   # ditto
#>               t             x             y             z
#> t -1.110223e-15  2.220446e-16 -5.551115e-17 -2.220446e-16
#> x  1.110223e-16  4.440892e-16 -3.747003e-16 -1.665335e-16
#> y -1.110223e-16 -3.469447e-16 -8.881784e-16 -3.885781e-16
#> z  0.000000e+00 -1.665335e-16 -3.885781e-16  2.220446e-16


## Define a random-ish Lorentz transformation
L <- boost(r3vel(1)) %*% boost(r3vel(1)) %*% boost(r3vel(1))

## check it:


if (FALSE)    # needs emulator package
quad.form(eta(), L)  # should be eta()
 # \dontrun{}

## More concisely:
is.consistent.boost(L)     # should be TRUE
#> [1] TRUE

## Decompose L into a rotation and a pure boost:
U <- orthog(L)
P <- pureboost(L)

L - U %*% P              # should be zero (L = UP)
#>               t             x             y             z
#> t  1.776357e-15  1.110223e-15 -4.440892e-16 -5.551115e-16
#> x -8.881784e-16 -6.106227e-16  4.440892e-16  3.330669e-16
#> y -1.776357e-15 -1.332268e-15  1.110223e-15  7.771561e-16
#> z  2.498002e-16  5.551115e-17  0.000000e+00  0.000000e+00
crossprod(U)               # should be identity (U is orthogonal)
#>               t            x             y             z
#> t  1.000000e+00 5.623273e-15 -4.568611e-15 -1.221157e-15
#> x  5.623273e-15 1.000000e+00  2.472227e-15  1.079557e-15
#> y -4.568611e-15 2.472227e-15  1.000000e+00 -3.532795e-16
#> z -1.221157e-15 1.079557e-15 -3.532795e-16  1.000000e+00
P - t(P)                   # should be zero (P is symmetric)
#>   t            x             y             z
#> t 0 0.000000e+00  0.000000e+00  0.000000e+00
#> x 0 0.000000e+00  0.000000e+00 -5.551115e-17
#> y 0 0.000000e+00  0.000000e+00  1.110223e-16
#> z 0 5.551115e-17 -1.110223e-16  0.000000e+00

## First row of P should be a consistent 4-velocity:
is.consistent.4vel(P[1,,drop=FALSE],give=TRUE)
#>             t 
#> -6.661338e-15 
```
