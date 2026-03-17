# Mirrors

Plane mirrors in special relativity

## Usage

``` r
reflect(P,m,ref=1)
```

## Arguments

- P:

  Vector of four-momenta

- m:

  Orientation of mirror, expressed as a three-vector

- ref:

  Coefficient of reflectivity of the mirror

## Value

Takes a four-momentum and returns the four-momentum after reflection.
Will handle objects or photons.

## Author

Robin K. S. Hankin

## See also

[`photon`](https://robinhankin.github.io/lorentz/reference/photon.md)

## Note

All four-momenta are measured in the rest frame of the mirror, but it is
easy to reflect from moving mirrors; see examples.

However, note that the `ref` argument is designed to work with photons
only, where it is conceptually the percentage of photons reflected and
not absorbed by the mirror. If `ref` is less than unity, odd results are
given for four momenta of nonzero restmass objects.

## Examples

``` r
## We will reflect some photons from an oblique mirror moving at half
## the speed of light.

## First create 'A', a bunch of photons all moving roughly along the x-axis:
A <- as.photon(as.3vel(cbind(0.9,runif(10)/1000,runif(10)/1000)))

## Now create 'm', a mirror oriented perpendicular to c(1,1,1):
m <- c(1,1,1)

## Reflect the photons in the mirror:
reflect(A,m)
#>       E       p_x        p_y        p_z
#>  [1,] 1 0.3321928 -0.6668874 -0.6670151
#>  [2,] 1 0.3322844 -0.6669715 -0.6668854
#>  [3,] 1 0.3327337 -0.6669269 -0.6667060
#>  [4,] 1 0.3326268 -0.6673622 -0.6663235
#>  [5,] 1 0.3323483 -0.6670075 -0.6668175
#>  [6,] 1 0.3329772 -0.6667594 -0.6667519
#>  [7,] 1 0.3332999 -0.6666554 -0.6666947
#>  [8,] 1 0.3325008 -0.6666548 -0.6670941
#>  [9,] 1 0.3326675 -0.6671699 -0.6664958
#> [10,] 1 0.3327153 -0.6671589 -0.6664829

## Reflect the photons in a series of mirrors:
A |> reflect(m) |> reflect(1:3) |> reflect(3:1) 
#>       E        p_x        p_y       p_z
#>  [1,] 1 -0.6468273 -0.7476385 0.1505029
#>  [2,] 1 -0.6466903 -0.7477518 0.1505287
#>  [3,] 1 -0.6464284 -0.7480372 0.1502354
#>  [4,] 1 -0.6461089 -0.7482207 0.1506953
#>  [5,] 1 -0.6466150 -0.7478175 0.1505259
#>  [6,] 1 -0.6464150 -0.7481070 0.1499449
#>  [7,] 1 -0.6462912 -0.7482694 0.1496683
#>  [8,] 1 -0.6468292 -0.7477146 0.1501163
#>  [9,] 1 -0.6462545 -0.7481351 0.1504958
#> [10,] 1 -0.6462322 -0.7481618 0.1504589


## To reflect from a moving mirror we need to transform to a frame in
## which the mirror is at rest, then transform back to the original
## frame.  First create B, a boost representing the mirror's movement
## along the x-axis at speed c/2:

B <- boost(as.3vel(c(0.5,0,0)))


## Transform to the mirror's rest frame:
A %*% t(B)    
#>               t         x            y            z
#>  [1,] 0.5773507 0.5773494 9.190760e-04 7.913814e-04
#>  [2,] 0.5773506 0.5773496 7.434578e-04 8.295925e-04
#>  [3,] 0.5773504 0.5773500 3.392551e-04 5.601556e-04
#>  [4,] 0.5773506 0.5773496 1.041656e-05 1.049097e-03
#>  [5,] 0.5773506 0.5773496 6.436446e-04 8.336927e-04
#>  [6,] 0.5773503 0.5773502 2.633771e-04 2.708369e-04
#>  [7,] 0.5773503 0.5773503 4.476737e-05 5.434626e-06
#>  [8,] 0.5773505 0.5773498 8.439073e-04 4.046268e-04
#>  [9,] 0.5773505 0.5773499 1.622607e-04 8.363282e-04
#> [10,] 0.5773505 0.5773499 1.254527e-04 8.014256e-04

## NB: in the above, take a transpose because the *rows* of A are 4-vectors.

## Then reflect the photons in the mirror:
reflect(A %*% t(B),m)
#>               E       p_x        p_y        p_z
#>  [1,] 0.5773507 0.1913095 -0.3851208 -0.3852485
#>  [2,] 0.5773506 0.1914012 -0.3852049 -0.3851188
#>  [3,] 0.5773504 0.1918504 -0.3851604 -0.3849395
#>  [4,] 0.5773506 0.1917435 -0.3855957 -0.3845570
#>  [5,] 0.5773506 0.1914650 -0.3852410 -0.3850510
#>  [6,] 0.5773503 0.1920939 -0.3849929 -0.3849854
#>  [7,] 0.5773503 0.1924166 -0.3848889 -0.3849282
#>  [8,] 0.5773505 0.1916176 -0.3848883 -0.3853276
#>  [9,] 0.5773505 0.1917842 -0.3854034 -0.3847293
#> [10,] 0.5773505 0.1918320 -0.3853924 -0.3847164


## Now transform back to the original rest frame (NB: active transformation):
A |> tcrossprod(B) |> reflect(m) |> tcrossprod(solve(B))
#>               t         x          y          z
#>  [1,] 0.7771197 0.5542388 -0.3851208 -0.3852485
#>  [2,] 0.7771726 0.5543446 -0.3852049 -0.3851188
#>  [3,] 0.7774317 0.5548632 -0.3851604 -0.3849395
#>  [4,] 0.7773702 0.5547399 -0.3855957 -0.3845570
#>  [5,] 0.7772094 0.5544182 -0.3852410 -0.3850510
#>  [6,] 0.7775722 0.5551443 -0.3849929 -0.3849854
#>  [7,] 0.7777585 0.5555169 -0.3848889 -0.3849282
#>  [8,] 0.7772974 0.5545944 -0.3848883 -0.3853276
#>  [9,] 0.7773936 0.5547868 -0.3854034 -0.3847293
#> [10,] 0.7774212 0.5548420 -0.3853924 -0.3847164

```
