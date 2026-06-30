# Direction cosines

Given a vector of three-velocities, returns their direction cosines

## Usage

``` r
cosines(u, drop = TRUE)
```

## Arguments

- u:

  A vector of three-velocities

- drop:

  Boolean, with default `TRUE` meaning to coerce return value from a
  one-row matrix to a vector, and `FALSE` meaning to consistently return
  a matrix

## Author

Robin K. S. Hankin

## Examples

``` r

cosines(r3vel(7))
#>                 x           y         z
#> [1,] -0.533315119 -0.13346830 0.8353210
#> [2,]  0.317219984  0.07102721 0.9456884
#> [3,]  0.743552008 -0.19968971 0.6381649
#> [4,] -0.582826072 -0.10451517 0.8058476
#> [5,] -0.733450293  0.65997647 0.1627321
#> [6,]  0.835570197  0.06072827 0.5460170
#> [7,]  0.003935698  0.13927406 0.9902461


cosines(r3vel(1),drop=TRUE)
#>         x         y         z 
#> 0.4819263 0.8122734 0.3285710 
cosines(r3vel(1),drop=FALSE)
#>               x         y          z
#> [1,] -0.4256414 0.3210249 -0.8460334
```
