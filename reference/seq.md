# seq method for three velocities

Simplified version of [`seq()`](https://rdrr.io/r/base/seq.html) for
three-velocities.

## Usage

``` r
# S3 method for class '3vel'
seq(from, to, len, ...)
```

## Arguments

- from,to:

  Start and end of sequence

- len:

  Length of vector returned

- ...:

  Further arguments (currently ignored)

## Details

`seq(a,b,n)` returns `a + t*(-b+a)` where `t` is numeric vector
`seq(from=0,to=1,len=n)`.

This definition is one of several plausible alternatives, but has the
nice property that the first and last elements are exactly equal to `a`
and `b` respectively.

## Author

Robin K. S. Hankin

## Examples

``` r
a <- as.3vel(c(4,5,6)/9)
b <- as.3vel(c(-5,6,8)/14)
x <- seq(a,b,len=9)

x[1]-a # should be zero
#> A vector of three-velocities (speed of light = 1)
#>      x y z
#> [1,] 0 0 0
x[9]-b # should be zero
#> A vector of three-velocities (speed of light = 1)
#>                x             y             z
#> [1,] 2.22201e-15 -8.428313e-16 -1.072694e-15


jj <- a + seq(0,1,len=9)*(b-a)

jj-x   # decidedly non-zero
#> A vector of three-velocities (speed of light = 1)
#>                 x         y         z
#>  [1,]  0.00000000 0.0000000 0.0000000
#>  [2,] -0.04838804 0.1181117 0.1533975
#>  [3,] -0.03232101 0.2418630 0.3086692
#>  [4,]  0.03217792 0.3559577 0.4477686
#>  [5,]  0.11675360 0.4488587 0.5584128
#>  [6,]  0.19150356 0.5159410 0.6371904
#>  [7,]  0.23782735 0.5594566 0.6884694
#>  [8,]  0.25335939 0.5854552 0.7200976
#>  [9,]  0.24656019 0.6002548 0.7393787

```
