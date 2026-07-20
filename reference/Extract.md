# Extract or replace parts of three-velocity

Extract or replace subsets of three-velocities

## Usage

``` r
# S3 method for class 'vec'
x[i, j, drop = TRUE, ...]
# S3 method for class '3vel'
x[i, j] <- value
# S3 method for class '4vel'
x[i, j] <- value
```

## Arguments

- x:

  A three-vector

- i,j:

  elements to extract or replace

- value:

  replacement value

- drop:

  Boolean, with default `TRUE` meaning to drop values

- ...:

  Further arguments, currently ignored

## Details

These methods (should) work as expected: an object of class `3vel` is a
three-column matrix with rows corresponding to three-velocities; a
single argument is interpreted as a row number. Salient use-cases are
`u[1:5] <- u[1]` and `u[1] <- 0`.

To extract a single component, pass a second index: `u[,1]` returns the
x- component of the three-velocity.

Extraction functions take a `drop` argument, except for `x[i]` which
returns a `vec` object.

Currently, `u[]` returns `u` but I am not sure this is desirable. Maybe
it should return `unclass(u)` or perhaps `c(unclass(u))`.

Use idiom `u[] <- x` to replace entries of `u` elementwise.

## Examples

``` r

u <- r3vel(10)
u[1:4]
#> A vector of three-velocities (speed of light = 1)
#>               x          y          z
#> [1,] -0.2556442  0.2543645 -0.5549431
#> [2,]  0.5820233  0.2968293  0.5875350
#> [3,]  0.1887402  0.8636937  0.1818922
#> [4,]  0.3821643 -0.1809655 -0.3982112
u[5:6] <- 0

u[7:8] <- u[1]

u[,1] <- 0.1
```
