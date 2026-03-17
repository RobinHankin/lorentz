# The Lorentz Transformation in Relativistic Physics

The Lorentz transformation in special relativity; also the gyrogroup
structure of three-velocities. Performs active and passive
transformations and has the ability to use units in which the speed of
light is not unity. Includes some experimental functionality for
celerity and rapidity. For general relativity, see the 'schwarzschild'
package.

## Details

The DESCRIPTION file: This package was not yet installed at build
time.  
Index: This package was not yet installed at build time.  

## Author

Robin K. S. Hankin \[aut, cre\] (ORCID:
\<https://orcid.org/0000-0001-5982-0415\>)

Maintainer: Robin K. S. Hankin \<hankin.robin@gmail.com\>

## References

- Ungar 2006. “Thomas precession: a kinematic effect...”. *European
  Journal of Physics*, 27:L17-L20.

- <https://www.youtube.com/watch?v=9Y9CxiukURw&index=68&list=PL9_n3Tqzq9iWtgD8POJFdnVUCZ_zw6OiB>

## Examples

``` r
u <- as.3vel(c(0.3,0.6,-0.1))  # u is a three-velocity
gam(u)                         # relativistic gamma term for u
#> [1] 1.360828
U <- as.4vel(u)                # U is a four-velocity
B1 <- boost(u)                 # B1 is the Lorentz transformation matrix for u
B1 %*% c(1,0,0,0)              # Lorentz transformation of zero 4-velocity (=-u)
#>         [,1]
#> t  1.3608276
#> x -0.4082483
#> y -0.8164966
#> z  0.1360828

B2 <- boost(as.3vel(c(-0.1,0.8,0.3)))  
B3 <- boost(as.3vel(c(-0.1,0.1,0.9)))  # more boosts

Bi <- B1 %*% B2  # Bi is the boost for successive Lorentz transformation


pureboost(Bi)      # Decompose Bi into a pure boost...
#>             t           x           y           z
#> t  3.78969964 -0.06713162 -3.54542995 -0.88726725
#> x -0.06713162  1.00094091  0.04969215  0.01243579
#> y -3.54542995  0.04969215  3.62439704  0.65677268
#> z -0.88726725  0.01243579  0.65677268  1.16436170
orthog(Bi)         # and an orthogonal matrix
#>               t             x             y             z
#> t  1.000000e+00 -8.606574e-15 -4.415801e-13 -1.116800e-13
#> x -1.027533e-13  9.794943e-01  1.895365e-01  6.831470e-02
#> y -4.562551e-13 -1.983854e-01  9.664761e-01  1.629943e-01
#> z -3.279628e-14 -3.513115e-02 -1.732047e-01  9.842591e-01

Bj <- B2 %*% B1    # B1 and B2 do not commute...

(B1 %*% B2) %*% B3 
#>            t          x          y          z
#> t  11.971728  1.0488714 -4.6614329 -10.931294
#> x  -2.357261  0.7672474  1.0039324   2.227135
#> y -11.332721 -1.2028617  4.6544280  10.311158
#> z  -2.887046 -0.2541382  0.2395334   3.035305
B1 %*% (B2 %*% B3)    # ...but composition *is* associative
#>            t          x          y          z
#> t  11.971728  1.0488714 -4.6614329 -10.931294
#> x  -2.357261  0.7672474  1.0039324   2.227135
#> y -11.332721 -1.2028617  4.6544280  10.311158
#> z  -2.887046 -0.2541382  0.2395334   3.035305



## Three velocities and the gyrogroup

## Create some random three-velocities:

u <- r3vel(10)
v <- r3vel(10)
w <- r3vel(10)

u+v
#> A vector of three-velocities (speed of light = 1)
#>                 x           y           z
#>  [1,] -0.56478934  0.25823932  0.49149635
#>  [2,]  0.34222078  0.49328155  0.26853820
#>  [3,]  0.81928617 -0.48504410  0.02809624
#>  [4,] -0.06886203  0.85086150 -0.50186720
#>  [5,] -0.89662990  0.21725228  0.12656127
#>  [6,] -0.66549350  0.17426286 -0.46591289
#>  [7,]  0.76717773  0.06379495 -0.61273061
#>  [8,]  0.68245485 -0.11176882  0.40877188
#>  [9,]  0.77223399  0.32665621 -0.48320928
#> [10,] -0.65375636  0.36263203  0.54726968
v+u        # Three-velocity addition is not commutative...
#> A vector of three-velocities (speed of light = 1)
#>                x          y           z
#>  [1,] -0.5629663  0.3130682  0.46076074
#>  [2,]  0.3231107  0.5216995  0.23660851
#>  [3,]  0.7071820 -0.5889199 -0.24567041
#>  [4,]  0.6605714  0.5145136 -0.52867678
#>  [5,] -0.6645414  0.6434610  0.10725765
#>  [6,] -0.5676908  0.3426581 -0.50063612
#>  [7,]  0.9468298  0.2513063 -0.09180836
#>  [8,]  0.4733079  0.1079067  0.64005243
#>  [9,]  0.3085770  0.7686311 -0.50052696
#> [10,] -0.5189814  0.4236121  0.64001137

u+(v+w)   # ... nor associative
#> A vector of three-velocities (speed of light = 1)
#>                x            y           z
#>  [1,] -0.1475623  0.796112386  0.41888092
#>  [2,]  0.0851946  0.935680929  0.31913792
#>  [3,]  0.8124212 -0.455730757 -0.05253253
#>  [4,] -0.0561954  0.851463111 -0.51949620
#>  [5,] -0.8049151  0.433754119 -0.35309972
#>  [6,] -0.7418702 -0.007149504 -0.49923309
#>  [7,]  0.8348374  0.058773808 -0.52618982
#>  [8,]  0.8569664 -0.222827177 -0.11727036
#>  [9,]  0.8262881  0.365942605 -0.40246178
#> [10,] -0.2715871  0.503695029  0.48807698
(u+v)+w 
#> A vector of three-velocities (speed of light = 1)
#>                  x           y           z
#>  [1,] -0.106783838  0.79515489  0.41054712
#>  [2,]  0.061286080  0.94386042  0.29878218
#>  [3,]  0.814280148 -0.43935546 -0.03085530
#>  [4,] -0.001697546  0.84255815 -0.53548132
#>  [5,] -0.797888806  0.26806231 -0.47923239
#>  [6,] -0.746013306  0.02691658 -0.51465350
#>  [7,]  0.835385914  0.04417690 -0.50303913
#>  [8,]  0.914693664 -0.21915463  0.06462596
#>  [9,]  0.805271704  0.40325896 -0.40763310
#> [10,] -0.316504550  0.45710983  0.38516756
```
