# Celerity and rapidity

The celerity and rapidity of an object (experimental functionality)

## Usage

``` r
# S3 method for class '3vel'
celerity(u)
# S3 method for class '4vel'
celerity(u)
celerity_ur(d)
# S3 method for class '3vel'
rapidity(u)
# S3 method for class '4vel'
rapidity(u)
rapidity_ur(d)
as.3cel(x)
cel_to_vel(x)
vel_to_cel(x)
```

## Arguments

- u,x:

  Speed: either a vector of speeds or a vector of three-velocities or
  four-velocities

- d:

  In functions `celerity_ur()` and `rapidity_ur()`, deficit of speed;
  speed of light minus speed of object

## Details

The celerity corresponding to speed \\u\\ is defined as \\u\gamma\\ and
the rapidity is \\c\cdot\mathrm{atanh}(u/c)\\.

Functions `celerity_ur()` and `rapidity_ur()` are used for the
ultrarelativistic case where speeds are very close to the speed of
light. Its argument `d` is the deficit, that is, \\d=c-v\\ where \\v\\
is the speed of the transformation. Algebraically,
`celerity_ur(c-v) == celerity(v)`, but if \\d=1-v/c\\ is small the
result of `celerity_ur()` is more accurate than that of `celerity()`.

Things get a bit sticky for celerity and rapidity if \\c\neq 1\\. The
guiding principle in the package is to give the celerity and rapidity
the same units as \\c\\, so if \\u\ll c\\ we have that all three of
`celerity(u)`, `rapidity(u)` and `u` are approximately equal. Note
carefully that, in contrast, \\\gamma\\ is dimensionless. Also observe
that `d` in functions `celerity_ur()` and `rapidity_ur()` has the same
units as \\c\\.

## Author

Robin K. S. Hankin

## See also

[`gam`](https://robinhankin.github.io/lorentz/reference/gam.md)

## Examples

``` r

u <- 0.1  # c=1
c(u,celerity(u),rapidity(u))
#> [1] 0.1000000 0.1005038 0.1003353

omgp <- 4.9e-24  # speed deficit of the Oh-My-God particle
c(celerity_ur(omgp),rapidity_ur(omgp))
#> [1] 3.194383e+11 2.718298e+01


sol(299792458)                 # use SI units
#> [1] 299792458
u <- 3e7  # ~0.1c
c(u,celerity(u),rapidity(u))
#> [1] 30000000 30151345 30100745


snail <- 0.00275
c(snail,celerity(snail),rapidity(snail))
#> [1] 0.00275 0.00275 0.00275


omgp <- omgp*sol() 
c(celerity_ur(omgp),rapidity_ur(omgp))
#> [1] 9.576519e+19 8.149252e+09


sol(1)
#> [1] 1
```
