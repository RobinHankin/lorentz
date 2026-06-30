# Four velocities

Create and test for four-velocities.

## Usage

``` r
as.4vel(u)
is.consistent.4vel(U, give=FALSE, TOL=1e-10)
inner4(U,V=U)
to3(U)
```

## Arguments

- u:

  A vector of three-velocities

- U,V:

  A vector of four-velocities

- give:

  In function `is.consistent.4vel()`, Boolean with `TRUE` meaning to
  return \\U\cdot U+c^2\\, which is zero for a four-velocity, and
  default `FALSE` meaning to return whether the four-velocity is
  consistent to numerical precision

- TOL:

  Small positive value used for tolerance

## Details

Function
[`as.4vel()`](https://robinhankin.github.io/lorentz/reference/fourvel.html)
takes a three-velocity and returns a four-velocity.

Given a four-vector \\V\\, function `inner4()` returns the Lorentz
invariant \\V^iV_i=\eta\_{ij}V^iV^j\\. This quantity is unchanged under
Lorentz transformations. Note that function `inner4()` works for any
four-vector, not just four-velocities. It will work for (eg) a
four-displacement, a four-momentum vector or a four-frequency. In
electromagnetism, we could have a four-current or a four-potential. If
\\U\\ is a four-velocity, then \\U^iU_i=-c^2\\; if \\U\\ is a
4-displacement, then \\U^iU_i\\ is the squared interval. If \\P\\ is the
four-momentum of a photon then \\P^iP_i=0\\.

Function `to3()` is a low-level helper function used when
[`as.3vel()`](https://robinhankin.github.io/lorentz/reference/threevel.md)
is given a four-velocity.

Function `is.consistent.4vel()` checks for four-velocities being
consistent in the sense that \\U^iU_i=-c^2\\. Giving this function a
vector, for example, `is.consistent.4vel(1:5)`, will return an error.

Compare the functions documented here with
[`boost()`](https://robinhankin.github.io/lorentz/reference/boost.md),
which returns a \\4\times 4\\ transformation matrix (which also includes
rotation information).

## Author

Robin K. S. Hankin

## See also

[`boost`](https://robinhankin.github.io/lorentz/reference/boost.md)

## Examples

``` r

a <- r3vel(10)
as.4vel(a)     # a four-velocity
#> A vector of four-velocities (speed of light = 1)
#>              t          x           y          z
#>  [1,] 1.905838 -0.3445010 -0.22786034  1.5689545
#>  [2,] 1.857507  1.3525765 -0.73604600  0.2812550
#>  [3,] 1.489169 -0.1213651  0.93583005  0.5719421
#>  [4,] 3.213603 -2.2073824 -0.49069857  2.0527842
#>  [5,] 1.677714 -0.8223220  0.79819196  0.7080975
#>  [6,] 8.496426 -0.8723675  8.27522066 -1.3960489
#>  [7,] 1.034815  0.1800695  0.03603805 -0.1926614
#>  [8,] 2.077521 -0.8530538 -0.50790033 -1.5265751
#>  [9,] 1.438241  0.6280927 -0.75501323  0.3224788
#> [10,] 4.247248  1.1533215  3.87295914  0.8421125

as.3vel(as.4vel(a))-a   # zero to numerical precision
#> A vector of three-velocities (speed of light = 1)
#>                   x             y             z
#>  [1,]  0.000000e+00 -2.520359e-17  0.000000e+00
#>  [2,]  0.000000e+00  0.000000e+00  0.000000e+00
#>  [3,]  7.693933e-18  0.000000e+00 -6.155146e-17
#>  [4,]  0.000000e+00  1.433193e-16 -5.732773e-16
#>  [5,] -1.562487e-16  0.000000e+00  7.812434e-17
#>  [6,]  0.000000e+00  0.000000e+00 -2.003654e-15
#>  [7,] -2.043377e-17  2.322019e-19  4.644038e-18
#>  [8,]  0.000000e+00  0.000000e+00 -2.395914e-16
#>  [9,]  0.000000e+00  0.000000e+00  0.000000e+00
#> [10,]  0.000000e+00  0.000000e+00  0.000000e+00

inner4(as.4vel(a))   #  -1 to numerical precision
#>  [1] -1 -1 -1 -1 -1 -1 -1 -1 -1 -1

stopifnot(all(is.consistent.4vel(as.4vel(a))))


## check Lorentz invariance of dot product:
U <- as.4vel(r3vel(10))
V <- as.4vel(r3vel(10))
B <- boost(as.3vel(1:3/10))

frame1dotprod <- inner4(U, V)
frame2dotprod <- inner4(U %*% B, V %*% B)
max(abs(frame1dotprod-frame2dotprod))  # zero to numerical precision
#> [1] 1.332268e-15
```
