# Speed of light and Minkowski metric

Getting and setting the speed of light

## Usage

``` r
sol(c)
eta(downstairs=TRUE)
ptm(to_natural=TRUE, change_time=TRUE)
```

## Arguments

- c:

  Scalar, speed of light. If missing, return the speed of light

- downstairs:

  Boolean, with default `TRUE` meaning to return the covariant metric
  tensor \\g\_{ij}\\ with two downstairs indices, and `FALSE` meaning to
  return the contravariant version \\g^{ij}\\ with two upstairs indices

- to_natural,change_time:

  Boolean, specifying the nature of the passive transformation matrix

## Details

In the context of an R package, the symbol “c” presents particular
problems. In the lorentz package, the speed of light is denoted “sol”,
for ‘speed of light’. You can set the speed of light with `sol(x)`, and
query it with `sol()`; see the examples. An infinite speed of light is
sometimes useful for Galilean transformations.

The speed of light is a global variable, governed by `options("c")`. If
`NULL`, define `c=1`. Setting `showSOL` to `TRUE` makes `sol()` change
the prompt to display the speed of light which might be useful.

Function `eta()` returns the Minkowski flat-space metric
\$\$\mathrm{diag}\left(-c^2,1,1,1\right).\$\$

Note that the top-left element of `eta()` is \\-c^2\\, not \\-1\\.

Function `ptm()` returns a passive transformation matrix that converts
displacement vectors to natural units (`to_natural=TRUE`) or from
natural units (`to_natural=FALSE`). Argument `change_time` specifies
whether to change the unit of time (if `TRUE`) or the unit of length (if
`FALSE`).

## Author

Robin K. S. Hankin

## Note

Typing “`sol(299792458)`” is a lot easier than typing
“`options("c"=299792458)`”, which is why the package uses the idiom that
it does.

In a R-devel discussion about options for printing, Martin Maechler
makes the following observation: “Good programming style for functions
according to my book is to have them depend only on their arguments, and
if a global option really (really? think twice!) should influence
behavior, there should be arguments of the function which have a default
determined by the global option”

I think he is right in general, but offer the observation that the speed
of light depends on the units chosen, and typically one fixes one's
units once and for all, and does not subsequently change them. This
would indicate (to me at least) that a global option would be
appropriate. Further, there *is* a default, \\c=1\\, which is returned
by `sol()` if the option is unset. This is not just a “default”, though:
it is used in the overwhelming majority of cases. Indeed, pedagogically
speaking, one learning objective from the package is that units in which
\\c\neq 1\\ are difficult, awkward, and unnatural. In the package R
code, the *only* place the speed of light option is accessed is via
`sol()`. Similar arguments are presented in the clifford package at
`signature.Rd`.

Looking again at Martin's observation he seems to be suggesting that
something along the lines of

        gam <- function(u, c=1){1/sqrt(1-u^2/c^2)}
      

But this is asking for trouble:

        c(gam(0.4,c=1),gam(0.4,c=10))
      

which is meaningless at best and misleading at worst.

## Examples

``` r
sol()                          # returns current speed of light
#> [1] 1
sol(299792458)                 # use SI units
#> [1] 299792458
sol()                          # speed of light now SI value
#> [1] 299792458

eta()                          # note [t,t] term
#>               [,1] [,2] [,3] [,4]
#> [1,] -8.987552e+16    0    0    0
#> [2,]  0.000000e+00    1    0    0
#> [3,]  0.000000e+00    0    1    0
#> [4,]  0.000000e+00    0    0    1
u <- as.3vel(c(100,200,300))   # fast terrestrial speed, but not relativistic
boost(u)                       # boost matrix practically Galilean
#>      t             x            y             z
#> t    1 -1.112650e-15 -2.22530e-15 -3.337950e-15
#> x -100  1.000000e+00  1.11265e-13  1.668975e-13
#> y -200  1.112650e-13  1.00000e+00  3.337950e-13
#> z -300  1.668975e-13  3.33795e-13  1.000000e+00
is.consistent.boost(boost(u))  # should be TRUE
#> [1] TRUE
sol(1)                         # revert to relativistic units
#> [1] 1
```
