gyrogroup: noncommutative and nonassociative Lorentz velocity addition in special relativity; Thomas precession
====================================

# Introduction

``The nonassociativity of Einstein's velocity addition is not widely
known''-- Ungar 2006.

The `gyrogroup` package furnishes some R-centric functionality for
manipulating three-velocities in the context of their being a
gyrogroup, and the calculation of Thomas precession.  Natural R idiom
may be used to manipulate vectors of three-velocities, although one
must be careful with brackets.

# Installation

To install the current development version use devtools:

    devtools::install_github("RobinHankin/gyrogroup")


# Features

### Creation of three velocities:

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
"small" in any sense.

Commutativity is replaced with gyrocommutatitivity:

```
R> (u+v) - gyr(u,v,v+u)
                 x             y             z
[1,]  0.000000e+00  5.122634e-16  6.670097e-18
[2,]  1.550444e-16  6.644760e-17 -1.771936e-16
[3,]  9.035809e-17  4.517904e-16  5.421485e-16
[4,] -1.484050e-16 -1.484050e-16 -3.710124e-17
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

    vignette("gyrogroup")
 
 

