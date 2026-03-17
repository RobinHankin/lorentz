# Classical mechanics; Newtonian approximation; infinite speed of light

The Lorentz transformations reduce to their classical limit, the
Galilean transformations, if speeds are low compared with \\c\\. Package
idiom for working in a classical framework is to use an infinite speed
of light: `sol(Inf)`. Here I show examples of this.

## Author

Robin K. S. Hankin

## See also

[`boost`](https://robinhankin.github.io/lorentz/reference/boost.md)

## Examples

``` r
sol(Inf)
#> [1] Inf
boost(as.3vel(1:3))
#>    t x y z
#> t  1 0 0 0
#> x -1 1 0 0
#> y -2 0 1 0
#> z -3 0 0 1
as.3vel(1:3) + as.3vel(c(-1,4,5))     # classical velocity addition
#> A vector of three-velocities (speed of light = Inf)
#>      x y z
#> [1,] 0 6 8
rot(as.3vel(1:3),as.3vel(c(-4,5,2)))  # identity matrix
#>   x y z
#> x 1 0 0
#> y 0 1 0
#> z 0 0 1


B <- boost(as.3vel(1:3))
orthog(B) %*% pureboost(B)  # should be B
#>       t x y z
#> [1,]  1 0 0 0
#> [2,] -1 1 0 0
#> [3,] -2 0 1 0
#> [4,] -3 0 0 1

sol(1)
#> [1] 1
```
