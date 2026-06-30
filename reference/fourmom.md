# Four momentum

Create and test for four-momentum

## Usage

``` r
# S3 method for class '4mom'
Ops(e1, e2)
# S3 method for class '4mom'
sum(..., na.rm=FALSE)
vel_to_4mom(U,m=1)
p_to_4mom(p,E=1)
as.4mom(x)
is.4mom(x)
fourmom_mult(P,n)
fourmom_add(e1,e2)
```

## Arguments

- x,P,e1,e2:

  Four-momentum

- p:

  Three-momentum

- E:

  Scalar; energy

- U:

  Object coerced to four-velocity

- m:

  Scalar; rest mass

- n:

  Multiplying factor

- ...,na.rm:

  Arguments sent to [`sum()`](https://rdrr.io/r/base/sum.html)

## Details

Four-momentum is a relativistic generalization of three-momentum, with
the object's energy as the first element. It can be defined as \\mU\\,
where \\m\\ is the rest mass and \\U\\ the four-velocity. Equivalently,
one can define four-momentum as \\(E/c,p_x,p_y,p_z)\\ where \\E\\ is the
energy and \\(p_x,p_y,p_z)\\ the three-momentum.

Function `vel_to_4mom()` converts three-velocity to four-momentum, and
function `p_to_4mom()`) converts a three-momentum to a four-momentum.

The function `Ops.4mom()` passes unary and binary arithmetic operators
“`+`”, “`-`” and “`*`” to the appropriate specialist function.

The package is designed so that natural R idiom may be used for
physically meaningful operations such as combining momenta of different
objects, using the conservation of four-momentum.

For the four-momentum of a photon, use
[`as.photon()`](https://robinhankin.github.io/lorentz/reference/photon.md).

## Author

Robin K. S. Hankin

## See also

[`boost`](https://robinhankin.github.io/lorentz/reference/boost.md),[`as.photon`](https://robinhankin.github.io/lorentz/reference/photon.md)

## Examples

``` r

# Define 5 random three velocities:
v <- r3vel(5)

# convert to four-velocity:
as.4vel(v)
#> A vector of four-velocities (speed of light = 1)
#>              t           x          y           z
#> [1,]  3.963503   3.4494055 -1.6725343  -0.1165708
#> [2,]  2.516389   1.5227965 -1.2846904  -1.1674224
#> [3,]  1.455142  -0.6358931  0.7466551   0.3944405
#> [4,] 65.694483 -42.8768595 22.8798705 -44.1910793
#> [5,]  1.408564   0.2486428  0.3392758   0.8983988

# Now convert 'v' to four-momentum, specifying rest mass:
vel_to_4mom(v)         # 4mom of five objects with 3vel v, all unit mass
#>              E         p_x        p_y         p_z
#> [1,]  3.963503   3.4494055 -1.6725343  -0.1165708
#> [2,]  2.516389   1.5227965 -1.2846904  -1.1674224
#> [3,]  1.455142  -0.6358931  0.7466551   0.3944405
#> [4,] 65.694483 -42.8768595 22.8798705 -44.1910793
#> [5,]  1.408564   0.2486428  0.3392758   0.8983988
vel_to_4mom(v,   1:5)  # 4mom of five objects with 3vel v, masses 1-5
#>               E         p_x       p_y          p_z
#> [1,]   3.963503    3.449405 -1.672534   -0.1165708
#> [2,]   5.032778    3.045593 -2.569381   -2.3348448
#> [3,]   4.365425   -1.907679  2.239965    1.1833216
#> [4,] 262.777930 -171.507438 91.519482 -176.7643171
#> [5,]   7.042819    1.243214  1.696379    4.4919940
vel_to_4mom(v[1],1:5)  # 4mom of five objects with same 3vel, masses 1..5
#>              E       p_x       p_y        p_z
#> [1,]  3.963503  3.449405 -1.672534 -0.1165708
#> [2,]  7.927006  6.898811 -3.345069 -0.2331416
#> [3,] 11.890510 10.348216 -5.017603 -0.3497124
#> [4,] 15.854013 13.797622 -6.690137 -0.4662832
#> [5,] 19.817516 17.247027 -8.362672 -0.5828540

# Now convert 'v' to four-momentum, specifying energy E:
p_to_4mom(v,E=1)
#>      E        p_x        p_y         p_z
#> [1,] 1  0.8702921 -0.4219838 -0.02941105
#> [2,] 1  0.6051515 -0.5105293 -0.46392762
#> [3,] 1 -0.4369974  0.5131150  0.27106676
#> [4,] 1 -0.6526706  0.3482769 -0.67267566
#> [5,] 1  0.1765222  0.2408665  0.63781197
p_to_4mom(v,E=10)   # slower
#>       E        p_x        p_y         p_z
#> [1,] 10  0.8702921 -0.4219838 -0.02941105
#> [2,] 10  0.6051515 -0.5105293 -0.46392762
#> [3,] 10 -0.4369974  0.5131150  0.27106676
#> [4,] 10 -0.6526706  0.3482769 -0.67267566
#> [5,] 10  0.1765222  0.2408665  0.63781197
p_to_4mom(v,E=100)  # even slower
#>        E        p_x        p_y         p_z
#> [1,] 100  0.8702921 -0.4219838 -0.02941105
#> [2,] 100  0.6051515 -0.5105293 -0.46392762
#> [3,] 100 -0.4369974  0.5131150  0.27106676
#> [4,] 100 -0.6526706  0.3482769 -0.67267566
#> [5,] 100  0.1765222  0.2408665  0.63781197

# Four-momentum of objects moving closely parallel to the x-axis:
P <- vel_to_4mom(as.3vel(c(0.8,0,0)) + r3vel(7,0.01))

reflect(P)
#>             E       p_x           p_y          p_z
#> [1,] 1.668214 -1.335230 -0.0043311211 -0.008946813
#> [2,] 1.663017 -1.328734 -0.0077414493  0.005678284
#> [3,] 1.671448 -1.339272 -0.0001123204  0.009358614
#> [4,] 1.657531 -1.321876 -0.0013696037  0.007094038
#> [5,] 1.675886 -1.344820  0.0016786591 -0.007088032
#> [6,] 1.659045 -1.323768 -0.0058049609  0.005737301
#> [7,] 1.677172 -1.346428  0.0023109195  0.005794075
reflect(P,c(1,1,1))
#>             E       p_x        p_y        p_z
#> [1,] 1.668214 0.4362248 -0.8946743 -0.8900586
#> [2,] 1.663017 0.4415360 -0.8794568 -0.8928765
#> [3,] 1.671448 0.4525883 -0.8865716 -0.8960425
#> [4,] 1.657531 0.4444416 -0.8760648 -0.8845284
#> [5,] 1.675886 0.4446672 -0.9018317 -0.8930650
#> [6,] 1.659045 0.4412111 -0.8767525 -0.8882947
#> [7,] 1.677172 0.4542125 -0.8945260 -0.8980091

sum(P)
#>             E      p_x        p_y         p_z
#> [1,] 11.67231 9.340129 0.01536988 -0.01762747
```
