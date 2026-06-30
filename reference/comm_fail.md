# Failure of commutativity and associativity using visual plots

Relativistic addition of three-velocities is neither commutative nor
associative, and the functions documented here show this visually.

## Usage

``` r
comm_fail1(u, v, bold=5, r=1)
comm_fail2(u, v, bold=5, r=1)
ass_fail(u, v, w, bold=5,r=1)
my_seg(u,start=as.3vel(0), bold=5, ...)
```

## Arguments

- u,v,w,start:

  Three velocities. Arguments `u` and `w` are single-element three
  velocities, argument `v` is a vector. See the examples

- bold:

  Integer specifying which vector element to be drawn in bold

- r:

  Radius of dotted green circle, defaulting to 1 (corresponding to
  \\c=1\\). Use `NA` to suppress plotting of circle

- ...:

  Further arguments, passed to
  [`arrows()`](https://rdrr.io/r/graphics/arrows.html)

## Value

These functions are called for their side-effect of plotting a diagram.

## Note

The vignette `lorentz` gives more details and interpretation of the
diagrams.

Function `my_seg()` is an internal helper function.

## Author

Robin K. S. Hankin

## Examples

``` r

u <- as.3vel(c(0.4,0,0))
v <- seq(as.3vel(c(0.4,-0.2,0)), as.3vel(c(-0.3,0.9,0)),len=20)
w <- as.3vel(c(0.8,-0.4,0))

comm_fail1(u=u, v=v)

comm_fail2(u=u, v=v)

  ass_fail(u=u, v=v, w=w, bold=10)

```
