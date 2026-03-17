# Print methods for three-velocities and four-velocities

Print methods for three-velocities

## Usage

``` r
# S3 method for class '3vel'
print(x, ...)
# S3 method for class '3cel'
print(x, ...)
# S3 method for class '4vel'
print(x, ...)
# S3 method for class '4mom'
print(x, ...)
```

## Arguments

- x:

  Vector of three-velocities

- ...:

  Further arguments, currently ignored

## Value

Returns a vector of three-velocities

## Author

Robin K. S. Hankin

## Examples

``` r
r3vel(10)
#> A vector of three-velocities (speed of light = 1)
#>                 x           y           z
#>  [1,]  0.38812540  0.02007322  0.60302098
#>  [2,] -0.46152456  0.16010487  0.11652775
#>  [3,] -0.28832967  0.84934024 -0.11627971
#>  [4,]  0.32878148 -0.49957297  0.78274283
#>  [5,] -0.06818971 -0.73394267  0.47048129
#>  [6,] -0.06005709  0.51551698 -0.57749384
#>  [7,]  0.64333460 -0.49377685  0.06284957
#>  [8,]  0.12702823 -0.44600854 -0.51322443
#>  [9,]  0.84131237  0.16035357  0.01878644
#> [10,]  0.37614195  0.66591597  0.06724417
```
