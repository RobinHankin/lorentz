# Combine vectors of three-velocities and four-velocities into a single vector

Combines its arguments recursively to form a vector of three velocities
or four velocities

## Usage

``` r
# S3 method for class '3vel'
c(...)
# S3 method for class '3cel'
c(...)
# S3 method for class '4vel'
c(...)
```

## Arguments

- ...:

  Vectors of three-velocities

## Details

Returns a vector of three-velocities or four-velocities. These are
stored as three- or four- column matrices; each row is a velocity.

Names are inherited from the behaviour of
[`cbind()`](https://rdrr.io/r/base/cbind.html), not
[`c()`](https://rdrr.io/r/base/c.html).

## Note

This function is used extensively in `inst/distributive_search.R`.

For “c” as in celerity or speed of light, see
[`sol()`](https://robinhankin.github.io/lorentz/reference/sol.md).

## Author

Robin K. S. Hankin

## See also

[`sol`](https://robinhankin.github.io/lorentz/reference/sol.md)

## Examples

``` r

c(r3vel(3),r3vel(6,0.99))
#> A vector of three-velocities (speed of light = 1)
#>                  x           y          z
#>  [1,] -0.007813303  0.39998619 -0.8823986
#>  [2,] -0.123863455 -0.14622609  0.2742510
#>  [3,]  0.380779144  0.20143196 -0.7216001
#>  [4,]  0.824010246  0.17288921 -0.5207844
#>  [5,]  0.156690536 -0.55531826  0.8044686
#>  [6,]  0.661657254 -0.37905604  0.6313685
#>  [7,] -0.377873965 -0.82508348  0.3956621
#>  [8,]  0.820129354 -0.01136688 -0.5543993
#>  [9,]  0.197529345 -0.85866191  0.4514221

```
