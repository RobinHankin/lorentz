# Photons

Various functionality to deal with the 4-momentum of a photon

## Usage

``` r
is.consistent.nullvec(N,TOL=1e-10)
as.photon(x,E=1)
```

## Arguments

- N:

  Four-momentum to be tested for nullness

- TOL:

  tolerance

- x:

  Vector of three-velocities

- E:

  Energy, a scalar

## Details

Returns the four-momentum of a photon.

## Author

Robin K. S. Hankin

## See also

[`4mom`](https://robinhankin.github.io/lorentz/reference/fourmom.md),[`reflect`](https://robinhankin.github.io/lorentz/reference/reflect.md)

## Examples

``` r
## A bunch of photons all approximately parallel to the x-axis:
as.photon(as.3vel(cbind(0.9,runif(10)/1000,runif(10)/1000)))
#>       E       p_x          p_y          p_z
#>  [1,] 1 0.9999996 4.879443e-04 6.919984e-04
#>  [2,] 1 0.9999998 2.026421e-04 6.609101e-04
#>  [3,] 1 0.9999997 8.703138e-05 8.255755e-04
#>  [4,] 1 0.9999998 1.974439e-05 5.825334e-04
#>  [5,] 1 0.9999995 8.090885e-04 5.520278e-04
#>  [6,] 1 0.9999994 1.078716e-03 1.635422e-05
#>  [7,] 1 0.9999993 5.946858e-04 1.064354e-03
#>  [8,] 1 0.9999999 1.849897e-04 5.034324e-04
#>  [9,] 1 0.9999994 6.463957e-05 1.090590e-03
#> [10,] 1 0.9999997 5.101543e-04 6.052278e-04


## mirror ball:
jj <- matrix(rnorm(30),10,3)
disco <- sweep(matrix(rnorm(30),10,3),1,sqrt(rowSums(jj^2)),`/`)
p <- as.photon(c(1,0,0))
reflect(p,disco)
#>   E         p_x         p_y         p_z
#> x 1  0.19344682  0.48433611 -0.85322732
#> x 1 -0.02565068  0.09771695  0.99488363
#> x 1  0.22462933 -0.50109245 -0.83573203
#> x 1 -0.15786155 -0.68186678  0.71423905
#> x 1 -0.13514534 -0.16415464 -0.97713305
#> x 1  0.66412284  0.10852636  0.73970459
#> x 1  0.99718547 -0.06151008  0.04286781
#> x 1  0.80930814 -0.08420989 -0.58131664
#> x 1  0.89037130  0.36362498 -0.27389015
#> x 1  0.45237354 -0.74684924 -0.48741603

table(reflect(p,disco)[,2]>0) # should be TRUE with probability sqrt(0.5)
#> 
#> FALSE  TRUE 
#>     3     7 

## relativistic  disco; mirror ball moves at 0.5c:

B <- boost(as.3vel(c(0.5,0,0)))
p |> tcrossprod(B) |> reflect(disco) |> tcrossprod(solve(B))
#>           t         x           y           z
#> x 0.7311489 0.4622979  0.27963158 -0.49261102
#> x 0.6581164 0.3162329  0.05641691  0.57439633
#> x 0.7415431 0.4830862 -0.28930586 -0.48251011
#> x 0.6140461 0.2280923 -0.39367597  0.41236611
#> x 0.6216182 0.2432364 -0.09477472 -0.56414803
#> x 0.8880409 0.7760819  0.06265772  0.42706865
#> x 0.9990618 0.9981236 -0.03551286  0.02474974
#> x 0.9364360 0.8728721 -0.04861860 -0.33562332
#> x 0.9634571 0.9269142  0.20993898 -0.15813055
#> x 0.8174578 0.6349157 -0.43119361 -0.28140978

```
