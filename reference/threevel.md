# Three velocities

Create and test for three-velocities, `3vel` objects.

## Usage

``` r
`3vel`(n)
threevel(n)
as.3vel(x)
is.3vel(x)
# S3 method for class 'vec'
length(x)
# S3 method for class 'vec'
names(x)
# S3 method for class 'vec'
names(x) <- value
```

## Arguments

- n:

  In function `3vel()`, number of three velocities to create

- x,value:

  Vectors of three-velocities

## Note

Class `vel` is a virtual class containing classes `3vel` and `4vel`.

Function `threevel()` is a convenience wrapper for `3vel()`.

## Author

Robin K. S. Hankin

## Examples

``` r

U <- r4vel(7)
as.4vel(as.3vel(U)) # equal to U, to numerical precision
#> A vector of four-velocities (speed of light = 1)
#>             t          x           y          z
#> [1,] 1.162723 -0.5652414 -0.11944376  0.1347603
#> [2,] 1.140353  0.3901290  0.02490881  0.3841675
#> [3,] 3.554731 -0.7579425 -0.63686459  3.2643587
#> [4,] 2.399008  0.2225749 -1.82140760  1.1782088
#> [5,] 1.173419 -0.1327862  0.58795266  0.1165838
#> [6,] 1.239273 -0.4347763  0.22239579 -0.5452584
#> [7,] 1.437438 -0.1658815  0.47244676 -0.9030528

x <- as.3vel(1:3/4)
u <- as.3vel(matrix(runif(30)/10,ncol=3))

names(u) <- letters[1:10]

x+u
#> A vector of three-velocities (speed of light = 1)
#>           x         y         z
#> a 0.2526502 0.5208521 0.7488033
#> b 0.2507003 0.5155580 0.7545696
#> c 0.2482366 0.5166806 0.7461948
#> d 0.2577497 0.5099816 0.7583707
#> e 0.2503484 0.4953416 0.7646267
#> f 0.2555145 0.5111537 0.7465111
#> g 0.2521355 0.5158320 0.7481054
#> h 0.2501674 0.5078836 0.7514922
#> i 0.2618863 0.5173780 0.7443586
#> j 0.2502199 0.4950421 0.7636984
u+x  # not equal
#> A vector of three-velocities (speed of light = 1)
#>           x         y         z
#> a 0.2517846 0.5495142 0.7283296
#> b 0.2455450 0.5331455 0.7439687
#> c 0.2419075 0.5421733 0.7300108
#> d 0.2656532 0.5147744 0.7523815
#> e 0.2460376 0.4763588 0.7779713
#> f 0.2633975 0.5271539 0.7325129
#> g 0.2521641 0.5380034 0.7323129
#> h 0.2476939 0.5171935 0.7459415
#> i 0.2801738 0.5416879 0.7200077
#> j 0.2461775 0.4765757 0.7766493


```
