

#' Failure of commutativity and associativity using visual plots
#' 
#' Relativistic addition of three-velocities is neither commutative nor
#' associative, and the functions documented here show this visually.
#' 
#' 
#' @aliases comm_fail comm_fail1 comm_fail2 ass_fail ass_fail my_seg
#' @param u,v,w,start Three velocities.  Arguments \code{u} and \code{w} are
#' single-element three velocities, argument \code{v} is a vector.  See the
#' examples
#' @param bold Integer specifying which vector element to be drawn in bold
#' @param r Radius of dotted green circle, defaulting to 1 (corresponding to
#' \eqn{c=1}).  Use \code{NA} to suppress plotting of circle
#' @param ... Further arguments, passed to \code{arrows()}
#' @return These functions are called for their side-effect of plotting a
#' diagram.
#' @note The vignette \code{lorentz} gives more details and interpretation of
#' the diagrams.
#' 
#' Function \code{my_seg()} is an internal helper function.
#' @author Robin K. S. Hankin
#' @examples
#' 
#' 
#' u <- as.3vel(c(0.4,0,0))
#' v <- seq(as.3vel(c(0.4,-0.2,0)), as.3vel(c(-0.3,0.9,0)),len=20)
#' w <- as.3vel(c(0.8,-0.4,0))
#' 
#' comm_fail1(u=u, v=v)
#' comm_fail2(u=u, v=v)
#'   ass_fail(u=u, v=v, w=w, bold=10)
#' 
#' 
NULL





#' Extract or replace parts of three-velocity
#' 
#' Extract or replace subsets of three-velocities
#' 
#' These methods (should) work as expected: an object of class \code{3vel} is a
#' three-column matrix with rows corresponding to three-velocities; a single
#' argument is interpreted as a row number. Salient use-cases are \code{u[1:5]
#' <- u[1]} and \code{u[1] <- 0}.
#' 
#' To extract a single component, pass a second index: \code{u[,1]} returns the
#' x- component of the three-velocity.
#' 
#' Extraction functions take a \code{drop} argument, except for \code{x[i]}
#' which returns a \code{vec} object.
#' 
#' Currently, \code{u[]} returns \code{u} but I am not sure this is desirable.
#' Maybe it should return \code{unclass(u)} or perhaps \code{c(unclass(u))}.
#' 
#' Use idiom \code{u[] <- x} to replace entries of \code{u} elementwise.
#' 
#' @aliases Extract.3vel extract.3vel [.vel [<-.vel [.3vel [<-.3vel [.4vel
#' [<-.4vel
#' @param x A three-vector
#' @param index elements to extract or replace
#' @param value replacement value
#' @examples
#' 
#' 
#' u <- r3vel(10)
#' u[1:4]
#' u[5:6] <- 0
#' 
#' u[7:8] <- u[1]
#' 
#' u[,1] <- 0.1
#' 
#' 
NULL





#' Four momentum
#' 
#' Create and test for four-momentum
#' 
#' 
#' Four-momentum is a relativistic generalization of three-momentum, with the
#' object's energy as the first element.  It can be defined as \eqn{mU}, where
#' \eqn{m} is the rest mass and \eqn{U} the four-velocity. Equivalently, one
#' can define four-momentum as \eqn{(E/c,p_x,p_y,p_z)}{(E/c,px,py,pz)} where
#' \eqn{E} is the energy and \eqn{(p_x,p_y,p_z)}{(px,py,pz)} the
#' three-momentum.
#' 
#' Function \code{vel_to_4mom()} converts three-velocity to four-momentum, and
#' function \code{p_to_4mom()}) converts a three-momentum to a four-momentum.
#' 
#' The function \code{Ops.4mom()} passes unary and binary arithmetic operators
#' \dQuote{\code{+}}, \dQuote{\code{-}} and \dQuote{\code{*}} to the
#' appropriate specialist function.
#' 
#' The package is designed so that natural idiom may be used for physically
#' meaningful operations such as combining momenta of different objects, using
#' the conservation of four-momentum.
#' 
#' For the four-momentum of a photon, use \code{as.photon()}.
#' 
#' @aliases Ops.4mom 4mom fourmom as.4mom sum.4mom fourmomentum four-momentum
#' 4momentum 4-momentum vel_to_4mom p_to_4mom as.4mom is.4mom fourmom_mult
#' fourmom_add
#' @param x,P,e1,e2 Four-momentum
#' @param p Three-momentum
#' @param E Scalar; energy
#' @param U Object coerced to four-velocity
#' @param m Scalar; rest mass
#' @param n Multiplying factor
#' @param ...,na.rm Arguments sent to \code{sum()}
#' @author Robin K. S. Hankin
#' @seealso \code{\link{boost}},\code{\link{as.photon}}
#' @examples
#' 
#' 
#' # Define 5 random three velocities:
#' v <- r3vel(5)
#' 
#' # convert to four-velocity:
#' as.4vel(v)
#' 
#' # Now convert 'v' to four-momentum, specifying rest mass:
#' vel_to_4mom(v)         # 4mom of five objects with 3vel v, all unit mass
#' vel_to_4mom(v,   1:5)  # 4mom of five objects with 3vel v, masses 1-5
#' vel_to_4mom(v[1],1:5)  # 4mom of five objects with same 3vel, masses 1..5
#' 
#' # Now convert 'v' to four-momentum, specifying energy E:
#' p_to_4mom(v,E=1)
#' p_to_4mom(v,E=10)   # slower
#' p_to_4mom(v,E=100)  # even slower
#' 
#' # Four-momentum of objects moving closely parallel to the x-axis:
#' P <- vel_to_4mom(as.3vel(c(0.8,0,0)) + r3vel(7,0.01))
#' 
#' reflect(P)
#' reflect(P,c(1,1,1))
#' 
#' sum(P)
#' 
#' 
NULL





#' Classical mechanics; Newtonian approximation; infinite speed of light
#' 
#' The Lorentz transforms reduce to their classical limit, the Galilean
#' transforms, if speeds are low compared with \eqn{c}.  Package idiom for
#' working in a classical framework is to use an infinite speed of light:
#' \code{sol(Inf)}.  Here I show examples of this.
#' 
#' 
#' @aliases galileo galilean Galileo Galilean classical newton Newton newtonian
#' Newtonian
#' @author Robin K. S. Hankin
#' @seealso \code{\link{boost}}
#' @examples
#' 
#' sol(Inf)
#' boost(as.3vel(1:3))
#' as.3vel(1:3) + as.3vel(c(-1,4,5))     # classical velocity addition
#' rot(as.3vel(1:3),as.3vel(c(-4,5,2)))  # identity matrix
#' 
#' 
#' B <- boost(as.3vel(1:3))
#' orthog(B) %*% pureboost(B)  # should be B
#' 
#' sol(1)
#' 
NULL





#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_title(\"#1\")}",
#' "lorentz")\Sexpr{tools:::Rd_package_title("lorentz")}
#' 
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_description(\"#1\")}",
#' "lorentz")\Sexpr{tools:::Rd_package_description("lorentz")}
#' 
#' 
#' The DESCRIPTION file:
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_DESCRIPTION(\"#1\")}",
#' "lorentz")\Sexpr{tools:::Rd_package_DESCRIPTION("lorentz")}
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_indices(\"#1\")}",
#' "lorentz")\Sexpr{tools:::Rd_package_indices("lorentz")}
#' 
#' @name lorentz-package
#' @aliases lorentz-package lorentz Lorentz gyrogroup
#' @docType package
#' @author
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_author(\"#1\")}",
#' "lorentz")\Sexpr{tools:::Rd_package_author("lorentz")}
#' 
#' Maintainer:
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_maintainer(\"#1\")}",
#' "lorentz")\Sexpr{tools:::Rd_package_maintainer("lorentz")}
#' @references \itemize{ \item Ungar 2006. \dQuote{Thomas precession: a
#' kinematic effect...}. \emph{European Journal of Physics}, 27:L17-L20. \item
#' \url{https://www.youtube.com/watch?v=9Y9CxiukURw&index=68&list=PL9_n3Tqzq9iWtgD8POJFdnVUCZ_zw6OiB}
#' }
#' @keywords package
#' @examples
#' 
#' 
#' u <- as.3vel(c(0.3,0.6,-0.1))  # u is a three-velocity
#' gam(u)                         # relativistic gamma term for u
#' U <- as.4vel(u)                # U is a four-velocity
#' B1 <- boost(u)                 # B1 is the Lorentz transform matrix for u
#' B1 %*% c(1,0,0,0)              # Lorentz transform of zero 4-velocity (=-u)
#' 
#' B2 <- boost(as.3vel(c(-0.1,0.8,0.3)))  
#' B3 <- boost(as.3vel(c(-0.1,0.1,0.9)))  # more boosts
#' 
#' Bi <- B1 %*% B2  # Bi is the boost for successive Lorentz transforms
#' 
#' 
#' pureboost(Bi)      # Decompose Bi into a pure boost...
#' orthog(Bi)         # and an orthogonal matrix
#' 
#' Bj <- B2 %*% B1    # B1 and B2 do not commute...
#' 
#' (B1 %*% B2) %*% B3 
#' B1 %*% (B2 %*% B3)    # ...but composition *is* associative
#' 
#' 
#' 
#' ## Three velocities and the gyrogroup
#' 
#' ## Create some random three-velocities:
#' 
#' u <- r3vel(10)
#' v <- r3vel(10)
#' w <- r3vel(10)
#' 
#' u+v
#' v+u        # Three-velocity addition is not commutative...
#' 
#' u+(v+w)   # ... nor associative
#' (u+v)+w 
#' 
#' 
NULL





#' Photons
#' 
#' Various functionality to deal with the 4-momentum of a photon
#' 
#' Returns the four-momentum of a photon.
#' 
#' @aliases photon as.photon light nullvec nullvector null vector
#' is.consistent.nullvec
#' @param N Four-momentum to be tested for nullness
#' @param TOL tolerance
#' @param x Vector of three-velocities
#' @param E Energy, a scalar
#' @author Robin K. S. Hankin
#' @seealso \code{\link{4mom}},\code{\link{reflect}}
#' @examples
#' 
#' 
#' ## A bunch of photons all approximately parallel to the x-axis:
#' as.photon(as.3vel(cbind(0.9,runif(10)/1000,runif(10)/1000)))
#' 
#' 
#' ## mirror ball:
#' jj <- matrix(rnorm(30),10,3)
#' disco <- sweep(matrix(rnorm(30),10,3),1,sqrt(rowSums(jj^2)),`/`)
#' p <- as.photon(c(1,0,0))
#' reflect(p,disco)
#' 
#' table(reflect(p,disco)[,2]>0) # should be TRUE with probability sqrt(0.5)
#' 
#' ## relativistic  disco; mirror ball moves at 0.5c:
#' 
#' B <- boost(as.3vel(c(0.5,0,0)))
#' p |> tcrossprod(B) |> reflect(disco) |> tcrossprod(solve(B))
#' 
#' 
#' 
NULL





#' The energy-momentum tensor
#' 
#' Various functionality to deal with the stress-energy tensor in special
#' relativity.
#' 
#' 
#' Function \code{perfectfluid()} returns the stress-energy tensor, with two
#' upstairs indices, for a perfect fluid with the conditions specified.  No
#' checking for physical reasonableness (eg the weak energy condition) is
#' performed: caveat emptor!
#' 
#' Function \code{dust()} is a (trivial) function that returns the
#' stress-energy tensor of a zero-pressure perfect fluid (that is, dust).
#' Function \code{photongas()} returns the stress-energy tensor of a photon
#' gas.  They are here for discoverability reasons; both are special cases of a
#' perfect fluid.
#' 
#' Functions \code{transform_dd()} et seq transform a second-rank tensor using
#' the Lorentz transform.  The letters \dQuote{u} or \dQuote{d} denote the
#' indices of the tensor being upstairs (contravariant) or downstairs
#' (covariant).  The stress-energy tensor is usually written with two upstairs
#' indices, so use \code{transform_uu()} to transform it.
#' 
#' Function \code{lower()} lowers both indices of a tensor with two upstairs
#' indices.  Function \code{raise()} raises two downstairs indices.  These two
#' functions have identical idiom but do not return identical values if
#' \eqn{c\neq 1}{c!=1}.
#' 
#' @aliases transform transform_dd transform_ud transform_uu raise lower
#' perfectfluid dust photongas SET stress stress-energy stress-energy-tensor
#' energy-momentum energy-momentum-tensor
#' @param TT A second-rank tensor with indices either downstairs-downstairs,
#' downstairs-upstairs, or upstairs-upstairs
#' @param B A boost matrix
#' @param rho,p,u Density, pressure, and four-velocity of the dust
#' @author Robin K. S. Hankin
#' @examples
#' 
#' 
#' perfectfluid(10,1)
#' 
#' u <- as.3vel(c(0.4,0.4,0.2))
#' 
#' ## In the following, LHS is stationary dust and RHS is dust moving at
#' ## velocity 'u', but transformed to a frame also moving at velocity 'u':
#' 
#' LHS <- dust(1)
#' RHS <- transform_uu(dust(1,u),boost(u))
#' max(abs(LHS-RHS))  # should be small
#' 
#' 
#' ## In the following, negative sign needed because active/passive
#' ## difference:
#' 
#' LHS <- dust(1,u)
#' RHS <- transform_uu(dust(1),boost(-u))
#' max(abs(LHS-RHS))  # should be small
#' 
#' ## Now test behaviour when  c!=1:
#' 
#' 
#' sol(299792458)
#' perfectfluid(1.225,101325) # air at STP
#' 
#' LHS <- transform_uu(perfectfluid(1.225,101325),boost(as.3vel(c(1000,0,0))))
#' RHS <- perfectfluid(1.225,101325) 
#' LHS-RHS  # should be small
#' 
#' sol(10)
#' u <- as.3vel(4:6)
#' LHS <- photongas(1,u)
#' RHS <- transform_uu(photongas(1),boost(-u))
#' LHS-RHS # should be small
#' 
#' 
#' 
#' B1 <- boost(r3vel(1)) %*% boost(r3vel(1))
#' B2 <- boost(r3vel(1)) %*% boost(r3vel(1))
#' LHS <- transform_uu(transform_uu(dust(1),B1),B2)
#' RHS <- transform_uu(dust(1),B2 %*% B1)   # note order
#' LHS-RHS  # should be small
#' 
#' 
#' ## remember to re-set c:
#' sol(1)
#' 
NULL



