---
title: 'Special relativity in R: the ``lorentz`` package '
authors:
- affiliation: 1
  name: Robin K. S. Hankin
  orcid: "0000-0001-5982-0415"
output:
  html_document:
    df_print: paged
bibliography: ref.bib
tags:
- special relativity
- Lorentz transformation
- Lorentz boosts
- Three velocity
- Four velocity
affiliations:
- index: 1
  name: Computing Science and Mathematics, University of Stirling
---

# Introduction: the Lorentz transformation in special relativity

In special relativity [@einstein1905], the Lorentz transformations
[@lorentz1904] supersede their classical equivalent, the Galilean
transformations.  Lorentz transformations operate on four-vectors such as the
four-velocity or four-potential and are usually operationalised as
multiplication by a $4\times 4$ matrix.  A Lorentz transformation takes the
components of an arbitrary four-vector as observed in one coordinate
system and returns the components observed in another system which is
moving at constant velocity with respect to the first.

The materials have been made publicly available at:
<https://github.com/RobinHankin/lorentz> and licensed under the
[GPL-3](https://www.gnu.org/licenses/gpl-3.0.en.html).  To install the
package, type 

```
install.packages("lorentz")
library("lorentz")
```

at the `R` [@rcore2022] command line.

# Statement of need

Einstein's theory of special relativity presents specific difficulties
for its teaching and learning [@prado2020].  One particularly
problematic concept is that of four velocity, defined as the
deriviative of an object's four-displacement with respect to its
proper time [@resnick1968].  Students are often left puzzled as to why
an object with three degrees of freedom is described using an object
with four components.  Observing (correctly) that the familiar
classical velocity addition law is incorrect for both three-velocities
and four-velocities in relativistic mechanics, students may reasonably
ask in what way four-velocities are preferable to three-velocities.

The `lorentz` package [@hankin2022_lorentz] furnishes a consistent
suite of computational functionality to give numerical illustration of
many concepts of special relativity, including manipulation of three-
and four- velocities.  It is accessible to undergraduates, being
written in the `R` programming language [@rcore2022] which will be
familiar to many physics students.  The package allows the user to
manipulate Lorentz boosts using familiar matrix multiplication, and in
addition incorporates an efficient and consistent interface to deal
with many aspects of undergraduate-level relativity including active
and passive transformations, four-momentum of photons, and in
particular the difficult and nonintuitive behaviour of units in which
$c\neq 1$ [such as S. I.].  The classical limit of
$c\longrightarrow\infty$ is discussed explicitly and computationally.
The package allows the user to employ efficient and natural vectorised
R idiom in the context of relativistic kinematics.

The package was originally written to facilitate investigation into
the nonassociative and noncommutative structure of
three-velocities [@ungar1997]; but the pedagogical impact of this is
to reinforce the primacy of four-vectors in relativity.  One student,
using the package, succinctly offered the observation that
``three-velocities suck": surely a profound insight.

# Previous related work

There are a few existing software tools for working with Lorentz
transformations, mostly developed in an educational context.  Early work
would include that of Horwitz et al. [-@horwitz1992], who describe
`relLab`, a system for building a range of *gendanken* experiments
in an interactive graphical environment.  The author asserts that it
runs on "any Macintosh computer with one megabyte of RAM or more" but
it is not clear whether the software is still available.  More modern
contributions would include the `OpenRelativity` toolkit
[@sherin2016] which simulates the effects of special relativity in the
`Unity` game engine.  There are also many excellent github repos
that provide functionality to create simple visual displays of Lorentz
transformations of events (@nick2019 is a good example), although
these are generally restricted to a single spatial dimension.

Educational models (in the sense of @possel2018) for Einstein's
general theory of relativity [@einstein1907] tend to be physical
[@possel2018]; but software examples would include the present
author's software [@hankin2021] for visualizing black holes and
gravitational radiation.

# The ``lorentz`` package: summary of high-level functionality

The `lorentz` package provides `R`-centric functionality for
Lorentz transformations.  It deals with formal Lorentz boosts and
converts between three-velocities and four-velocities.  Computational
support for the nonassociative and noncommutative structure of
relativistic three-velocity addition is included.  Some functionality
for relativistic transformation of tensors of order 2 such as the
stress-energy tensor is given, with examples.  In the package, the
speed of light is 1 by default, but is user-settable and the classical
limit is recovered by setting $c=\infty$.  Both passive and active
transformations are implemented.  An extensive heuristic vignette
detailing package idiom is included; to view this, type
"`vignette("lorentz")`" at the `R` command line.  There does not
seem to be a known relativistic generalization of the classical
distributive law $r\left({\bf u} + {\bf v}\right)=r{\bf u} + r{\bf v}$
[@ungar1997].  Ungar states that ``It is hoped that one day [such a
generalization] will be discovered.  If exists, it is expected to be
the standard distributive law relaxed by Thomas gyration in some
unexpected way".  The package is used to execute a systematic sweep
through possible distributive laws consistent with Ungar's suggestion,
unfortunately without success.

# References
