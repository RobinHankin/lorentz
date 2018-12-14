The lorentz R package: special relativity 
====================================

# Introduction

``The nonassociativity of Einstein's velocity addition is not widely
known''-- Ungar 2006.

The `lorentz` package furnishes some R-centric functionality for
special relativity.  Lorentz transformations of four-vectors are
handled and some functionality for the stress energy tensor is given.

The package also works with three-velocities in the context of their
being a gyrogroup.  Natural R idiom may be used to manipulate vectors
of three-velocities, although one must be careful with brackets.

# Installation

To install the most recent stable version on CRAN, use ```install.packages()```
at the R prompt:

```
R> install.packages("lorentz")
```


To install the current development version use ```devtools```:

```
R> devtools::install_github("RobinHankin/lorentz")
```

# Features

Natural R idiom can be used to define three-velocities,
four-velocities, and Lorentz transformations as four-by-four matrices.


```
R> u <- as.3vel(c(0.6,0,0))  # define a three-velocity, 0.6c to the right
R> u
       x y z
[1,] 0.6 0 0

R> as.4vel(u)    # convert to a four-velocity:
        t    x y z
[1,] 1.25 0.75 0 0

R> gam(u)  # calculate the gamma term
[1] 1.25

R> B <- boost(as.3vel(c(0.6,0,0))) # give the Lorentz transformation
R> B
      t     x y z
t  1.25 -0.75 0 0
x -0.75  1.25 0 0
y  0.00  0.00 1 0
z  0.00  0.00 0 1

R> B %*% (1:4)  # Lorentz transform of an arbitrary four-vector
   [,1]
t -0.25
x  1.75
y  3.00
z  4.00 
```

The package is fully vectorized and includes functionality to convert
between three-velocities and four-velocities:

```
R> set.seed(0)
R> options(digits=3)
R> # generate 5 random three-velocities:
R> (u <- r3vel(5))
          x       y      z
[1,]  0.230  0.0719  0.314
[2,] -0.311  0.4189 -0.277
[3,] -0.185  0.5099 -0.143
[4,] -0.739 -0.4641  0.129
[5,] -0.304 -0.2890  0.593

R> # calculate the gamma correction term:
R> gam(u)
[1] 1.09 1.24 1.21 2.13 1.46

R> # add a velocity of 0.9c in the x-direction:
R> v <- as.3vel(c(0.9,0,0))
R> v+u
         x      y      z
[1,] 0.936  0.026  0.113
[2,] 0.818  0.253 -0.168
[3,] 0.858  0.267 -0.075
[4,] 0.480 -0.605  0.168
[5,] 0.820 -0.174  0.356

R> # convert u to a four-velocity:
R> as.4vel(u)
        t      x       y      z
[1,] 1.09  0.250  0.0783  0.341
[2,] 1.24 -0.385  0.5190 -0.343
[3,] 1.21 -0.223  0.6160 -0.173
[4,] 2.13 -1.571 -0.9862  0.273
[5,] 1.46 -0.443 -0.4209  0.864

R> # use four-velocities to effect the same transformation:
R> w <- as.4vel(u) %*% boost(-v)
R> as.3vel(w)
         x      y      z
[1,] 0.936  0.026  0.113
[2,] 0.818  0.253 -0.168
[3,] 0.858  0.267 -0.075
[4,] 0.480 -0.605  0.168
[5,] 0.820 -0.174  0.356
```


### Creation of three velocities:

Three-velocites behave in interesting and counter-intuitive ways.


```
R> u <- as.3vel(c(0.2,0.4,0.1))   # single three-velocity
R> v <- r3vel(4,0.9)              # 4 random three-velocities with speed 0.9
R> w <- as.3vel(c(-0.5,0.1,0.3))  # single three-velocity
```

The three-velocity addition law is given by Ungar.

Then we can see that velocity addition is not commutative:

```
R> u+v
              x          y           z
[1,]  0.4727763 -0.6658390 -0.03265162
[2,] -0.3419352  0.6156001 -0.56433084
[3,] -0.7009057 -0.2123052  0.35080439
[4,] -0.5206572  0.5901378  0.46734052
R> v+u
              x          y           z
[1,]  0.3266974 -0.7458261 -0.07026683
[2,] -0.4809986  0.3973648 -0.65199233
[3,] -0.6860543 -0.3471104  0.26124665
[4,] -0.7027076  0.3813314  0.44558058
R> (u+v)-(v+u)
              x         y          z
[1,]  0.2554480 0.1152058 0.06165911
[2,]  0.2287210 0.4937333 0.10337471
[3,] -0.0410193 0.2173606 0.15599707
[4,]  0.2937451 0.4996093 0.10767540
R>
```

Observe that the difference between ```u+v``` and ```v+u``` is not
"small" in any sense. Commutativity is replaced with gyrocommutatitivity:

```
# Compare two different ways of calculating the same thing:
R> (u+v) - gyr(u,v,v+u)  
                 x             y             z
[1,]  0.000000e+00  5.122634e-16  6.670097e-18
[2,]  1.550444e-16  6.644760e-17 -1.771936e-16
[3,]  9.035809e-17  4.517904e-16  5.421485e-16
[4,] -1.484050e-16 -1.484050e-16 -3.710124e-17

# The other way round:
R> (v+u) - gyr(v,u,u+v)
                 x             y             z
[1,]  1.195281e-15  2.390563e-15 -2.027709e-16
[2,]  4.208348e-16 -1.107460e-17 -6.644760e-16
[3,] -2.108355e-16 -1.505968e-16  3.614324e-16
[4,]  1.558252e-15  1.929264e-15 -1.224341e-15
R>
``` 

(that is, zero to numerical accuracy)

### Nonassociativity

It would be reasonable to expect that ```u+(v+w)==(u+v)+w```.
However, this is not the case:
 
```
R> ((u+v)+w) - (u+(v+w))
                 x           y            z
[1,]  0.0006813334  0.11430397  0.041607715
[2,] -0.0173238864 -0.03804107  0.009219861
[3,] -0.0502024995 -0.12966668 -0.039637713
[4,] -0.1385891474 -0.16729878 -0.017400840
R>
``` 

(that is, significant departure from associativity).
Associativity is replaced with gyroassociativity:

```
R> (u+(v+w)) - ((u+v)+gyr(u,v,w))
                 x             y            z
[1,] -1.274920e-15 -3.399786e-15 0.000000e+00
[2,] -3.464734e-16  0.000000e+00 3.464734e-16
[3,] -1.064220e-15 -7.094802e-16 5.321101e-16
[4,] -6.675393e-16  0.000000e+00 0.000000e+00
R> ((u+v)+w) - (u+(v+gyr(v,u,w)))
                 x             y             z
[1,] -3.118327e-15 -5.345703e-15  0.000000e+00
[2,] -7.108603e-16  3.554301e-16  3.554301e-16
[3,] -2.072438e-16 -8.289750e-16  0.000000e+00
[4,]  9.646629e-16  2.572434e-15 -1.929326e-15
R> 
```

(zero to numerical accuracy).


# References

The most concise reference is

*  A. A. Ungar 2006. _Thomas precession: a kinematic effect of the algebra of Einstein's velocity addition law.  Comments on "Deriving relativistic momentum and energy: II,  Three-dimensional case_.  European Journal of Physics, 27:L17-L20


# Further information
For more detail, see the package vignette

    vignette("lorentz")
 
 

