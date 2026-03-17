# Gyr function

Relativistic addition of three velocities

## Usage

``` r
gyr(u, v, x)
gyr.a(u, v, x)
gyrfun(u, v)
```

## Arguments

- u,v,x:

  Three-velocities, objects of class `3vel`

## Details

Function `gyr(u,v,x)` returns the three-vector \\\mathrm{gyr}\[u,v\]x\\.

Function `gyrfun(u,v)` returns a function that returns a three-vector;
see examples.

The speed of light (1 by default) is not used directly by these
functions; set it with
[`sol()`](https://robinhankin.github.io/lorentz/reference/sol.md).

## References

- Ungar 2006. “Thomas precession: a kinematic effect of the algebra of
  Einstein's velocity addition law. Comments on ‘Deriving relativistic
  momentum and energy: I. Three-dimensional case’”. European Journal of
  Physics, 27:L17-L20.

- Sbitneva 2001. “Nonassociative geometry of special relativity”.
  International Journal of Theoretical Physics, volume 40, number 1,
  pages 359–362

## Author

Robin K. S. Hankin

## Note

Function `gyr()` is slightly faster than `gyr.a()`, which is included
for pedagogical reasons.

Function `gyr()` is simply

`add3(neg3(add3(u,v)),add3(u,add3(v,x)))`

while function `gyr.a()` uses the slower but more transparent idiom

` -(u+v) + (u+(v+x)) `

## Examples

``` r

u <- r3vel(10)
v <- r3vel(10)
w <- r3vel(10)

x <- as.3vel(c(0.4,0.1,-0.5))
y <- as.3vel(c(0.1,0.2,-0.7))
z <- as.3vel(c(0.2,0.3,-0.1))


gyr(u,v,x)  # gyr[u,v]x
#> A vector of three-velocities (speed of light = 1)
#>                 x            y          z
#>  [1,]  0.29688244 -0.284585217 -0.5008713
#>  [2,]  0.36875817  0.065856906 -0.5288481
#>  [3,]  0.53155065  0.074581061 -0.3631688
#>  [4,] -0.04404694 -0.175241426 -0.6223747
#>  [5,]  0.38537036  0.006363217 -0.5210079
#>  [6,]  0.47435109  0.170964952 -0.4071388
#>  [7,]  0.20106734  0.338903650 -0.5145058
#>  [8,] -0.10634811 -0.387125922 -0.5087471
#>  [9,]  0.20797919  0.015319935 -0.6136041
#> [10,]  0.45819629 -0.136356740 -0.4375649

f <- gyrfun(u,v)
g <- gyrfun(v,u)

f(x)
#> A vector of three-velocities (speed of light = 1)
#>                 x            y          z
#>  [1,]  0.29688244 -0.284585217 -0.5008713
#>  [2,]  0.36875817  0.065856906 -0.5288481
#>  [3,]  0.53155065  0.074581061 -0.3631688
#>  [4,] -0.04404694 -0.175241426 -0.6223747
#>  [5,]  0.38537036  0.006363217 -0.5210079
#>  [6,]  0.47435109  0.170964952 -0.4071388
#>  [7,]  0.20106734  0.338903650 -0.5145058
#>  [8,] -0.10634811 -0.387125922 -0.5087471
#>  [9,]  0.20797919  0.015319935 -0.6136041
#> [10,]  0.45819629 -0.136356740 -0.4375649
f(r3vel(10))
#> A vector of three-velocities (speed of light = 1)
#>                 x            y           z
#>  [1,]  0.07255247 -0.009937689 -0.97804879
#>  [2,] -0.05467681  0.171111420 -0.87218568
#>  [3,]  0.08386887  0.785488929  0.08851038
#>  [4,]  0.41152071  0.404793224 -0.56714449
#>  [5,]  0.32568173  0.407708661  0.36527154
#>  [6,]  0.55753999 -0.309549737 -0.62490156
#>  [7,]  0.19213693 -0.754186347 -0.61085947
#>  [8,] -0.69261374 -0.233064131  0.47417197
#>  [9,] -0.14808868 -0.914166267  0.03806476
#> [10,]  0.16215162  0.032779754 -0.38423240

f(g(x)) - x              # zero, by eqn 9
#> A vector of three-velocities (speed of light = 1)
#>                   x             y             z
#>  [1,]  2.320940e-15  7.393511e-15 -5.742533e-16
#>  [2,]  2.919121e-15 -6.705604e-15 -1.043227e-14
#>  [3,] -2.078079e-13 -9.932787e-14 -2.452540e-14
#>  [4,] -1.177219e-14 -1.433240e-14  1.086296e-14
#>  [5,] -5.981805e-16 -6.041623e-16 -2.392722e-17
#>  [6,] -1.028870e-15 -1.130561e-15  1.603124e-15
#>  [7,] -3.349811e-16 -1.196361e-17  2.153450e-16
#>  [8,] -3.100489e-13 -6.743349e-13 -8.060363e-13
#>  [9,]  2.464504e-15  3.050721e-16 -1.890250e-15
#> [10,] -3.947991e-15  2.841357e-15  7.178166e-17
g(f(x)) - x              # zero, by eqn 9
#> A vector of three-velocities (speed of light = 1)
#>                   x             y             z
#>  [1,] -2.105595e-15 -1.205932e-14 -1.272928e-14
#>  [2,]  5.263988e-15 -9.618743e-15 -1.004943e-14
#>  [3,] -1.520814e-13 -5.740738e-14 -2.316155e-14
#>  [4,] -2.608067e-14 -3.953973e-15  9.499106e-15
#>  [5,] -9.570888e-17 -9.570888e-17 -2.871266e-16
#>  [6,]  4.067627e-16  4.665808e-16  2.153450e-16
#>  [7,] -5.981805e-16 -5.981805e-18  4.067627e-16
#>  [8,]  1.237994e-13  4.074626e-13 -5.915527e-13
#>  [9,]  7.895983e-16  2.691812e-16 -5.024716e-16
#> [10,]  3.589083e-16  3.002866e-15 -2.249159e-15
(x+y) - f(y+x)           # zero by eqn 10
#> A vector of three-velocities (speed of light = 1)
#>                  x           y           z
#>  [1,]  0.441410083  0.70243973 -0.47007056
#>  [2,]  0.438160812 -0.02483539  0.08199839
#>  [3,] -0.280162799 -0.29800937 -0.50249057
#>  [4,]  0.786121767  0.36245653 -0.42410272
#>  [5,]  0.386826527  0.31030591  0.09773939
#>  [6,]  0.004981965 -0.37368903 -0.20381558
#>  [7,]  0.715022430 -0.35065032 -0.39790221
#>  [8,]  0.607567474  0.47310663 -0.61800986
#>  [9,]  0.786071516  0.10098613 -0.07005284
#> [10,] -0.131355795  0.43315800 -0.10110650
(u+(v+w)) - ((u+v)+f(w)) # zero by eqn 11
#> A vector of three-velocities (speed of light = 1)
#>                   x             y             z
#>  [1,]  0.000000e+00  0.000000e+00  4.109056e-13
#>  [2,]  1.804276e-14  1.443421e-13  1.443421e-13
#>  [3,]  4.665010e-14  2.332505e-14  4.665010e-14
#>  [4,]  1.514007e-14  1.514007e-14 -5.677528e-15
#>  [5,] -1.058779e-15 -2.117558e-15  0.000000e+00
#>  [6,]  1.216292e-16  4.865166e-16  0.000000e+00
#>  [7,]  0.000000e+00  4.918352e-16 -6.147940e-17
#>  [8,]  0.000000e+00  0.000000e+00  0.000000e+00
#>  [9,]  0.000000e+00  0.000000e+00 -4.753304e-16
#> [10,]  0.000000e+00  0.000000e+00 -1.493291e-16


# Following taken from Sbitneva 2001:

rbind(x+(y+(x+z))  ,   (x+(y+x))+z)   # left Bol property
#>           [,1]      [,2]       [,3]
#> [1,] 0.4894115 0.2314344 -0.8300767
#> [2,] 0.4894115 0.2314344 -0.8300767
rbind((x+y)+(x+y)  ,   x+(y+(y+x)))   # left Bruck property
#>          [,1]     [,2]       [,3]
#> [1,] 0.432766 0.209512 -0.8738679
#> [2,] 0.432766 0.209512 -0.8738679


sol(299792458)   # speed of light in SI
#> [1] 299792458
as.3vel(c(1000,3000,1000)) + as.3vel(c(1000,3000,1000))
#> A vector of three-velocities (speed of light = 299792458)
#>         x    y    z
#> [1,] 2000 6000 2000
## should be close to Galilean result

sol(1)   # revert to default c=1
#> [1] 1
```
