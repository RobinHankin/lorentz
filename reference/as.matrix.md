# Coerce 3-vectors and 4-vectors to a matrix

Coerce 3-vectors and 4-vectors to a matrix. A convenience wrapper for
[`unclass()`](https://rdrr.io/r/base/class.html)

## Usage

``` r
# S3 method for class '3vel'
as.matrix(x, ...)
# S3 method for class '4vel'
as.matrix(x, ...)
```

## Arguments

- x:

  Object of class `3vel` or `4vel`

- ...:

  Further arguments (currently ignored)

## Note

Coercing to a matrix simply removes the S3 class `3vel` or `4vel`. To
convert a three-velocity or a four-velocity to a \\4\times 4\\ matrix,
use
[`boost()`](https://robinhankin.github.io/lorentz/reference/boost.md)

## See also

[`boost`](https://robinhankin.github.io/lorentz/reference/boost.md)

## Author

Robin K. S. Hankin

## Examples

``` r
as.matrix(r3vel(5))
#>                x            y           z
#> [1,]  0.10045332  0.005487752  0.92505522
#> [2,] -0.19372407 -0.636199069 -0.65540611
#> [3,] -0.63628981  0.375232087  0.02743413
#> [4,]  0.07595006  0.427931096  0.41689644
#> [5,]  0.12864218 -0.660473984  0.32649312
as.matrix(r4vel(5))
#>             t           x          y          z
#> [1,] 1.644260 -1.18754951 -0.3146242 0.44082570
#> [2,] 1.421469 -0.71781477  0.7106876 0.01545716
#> [3,] 2.529112  0.15856601  2.1945818 0.74503490
#> [4,] 1.822870 -0.98407554  1.1632537 0.03594209
#> [5,] 1.953613 -0.02855582  1.2438818 1.12629806
```
