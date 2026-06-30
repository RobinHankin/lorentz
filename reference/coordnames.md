# Coordinate names for relativity

Trivial function to set coordinate names to “`t`”, “`x`”, “`y`”, “`z`”.

## Usage

``` r
coordnames(...)
flob(x)
```

## Arguments

- ...:

  Further arguments, currently ignored

- x:

  A matrix

## Details

Function `coordnames()` simply returns the character string
`c("t","x","y","z")`. It may be overwritten. Function `flob()` sets the
row and columnnames of a \\4\times 4\\ matrix to `coordnames()`.

## Author

Robin K. S. Hankin

## Note

If anyone can think of a better name than `flob()` let me know.

## Examples

``` r

coordnames()
#> [1] "t" "x" "y" "z"

flob(diag(3))
#>   x y z
#> x 1 0 0
#> y 0 1 0
#> z 0 0 1
flob(matrix(1,4,4))
#>   t x y z
#> t 1 1 1 1
#> x 1 1 1 1
#> y 1 1 1 1
#> z 1 1 1 1

## You can change the names if you wish:
coordnames <- function(x){letters[1:4]}
flob(outer(1:4,1:4))
#>   t x  y  z
#> t 1 2  3  4
#> x 2 4  6  8
#> y 3 6  9 12
#> z 4 8 12 16
```
