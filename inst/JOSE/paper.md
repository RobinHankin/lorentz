---
title: 'Special relativity in R: the ``lorentz`` package '
authors:
- affiliation: 1
  name: Robin K. S. Hankin
  orcid: 0000-0001-5982-0415
date: "September 05, 2019"
output: pdf_document
bibliography: ref.bib
tags:
- special relativity
- Lorentz transform
- Lorentz boosts
- Three velocity
- Four velocity
affiliations:
- index: 1
  name: Auckland University of Technology
---

# Introduction: the Lorentz transform in special relativity

In special relativity [@einstein1905], the Lorentz transforms
[@lorentz1904] supercede their classical equivalent, the Galilean
transforms.  Lorentz transforms operate on four-vectors such as the
four-velocity or four-potential and are usually operationalised as
multiplication by a $4\times 4$ matrix.  A Lorentz transform takes the
components of an arbitrary four-vector as observed in one coordinate
system and returns the components observed in another system which is
moving at constant velocity with respect to the first.

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
addition incorporates in an efficient and consistent interface to deal
with many aspects of undergraduate-level relativity including active
and passive transforms, four-mometum of photons, and in particular the
difficult and nonintuitive behaviour of units in which $c\neq 1$ [such
as S. I.].  The classical limit of $c=\infty$ is discussed explicitly
and computationally.  The package allows the user to employ efficient
and natural vectorised R idiom in the context of relativistic
kinematics.

The package was originally written to facilitate investigation into
the nonassociative and noncommutative "gyrogroup" structure of
three-velocities [@ungar1997]; but the pedagogical impact of this is
to reinforce the primacy of four-vectors in relativity.  One student,
using the package, succinctly offers the observation that
``three-velocities suck": surely a profound insight.

# Previous related work

There are a few existing software tools for working with Lorentz
transforms, mostly developed in an educational context.  Early work
would include that of Horwitz et al. [-@horwitz1992], who describe
``relLab``, a system for building a range of *gendanken* experiments
in an interactive graphical environment.  The author asserts that it
runs on "any Macintosh computer with one megabyte of RAM or more" but
it is not clear whether the software is still available.  More modern
contributions would include the ``OpenRelativity`` toolkit
[@sherin2016] which simulates the effects of special relativity in the
``Unity`` game engine.  There are also many excellent github repos
that provide functionality to create simple visual displays of Lorentz
transforms of events (eg
```https://github.com/nick1627/RelativityVisualisation```) although
these are generally restricted to a single spatial dimension.

Educational models (in the sense of @possel2018) for Einstein's
general theory of relativity [@einstein1907] tend to be physical
[@possel2018]; but software examples would include the present
author's software [@hankin2021] for visualizing black holes and
gravitational radiation.

# The ``lorentz`` package: summary of high-level functionality

The ``lorentz`` package provides ``R``-centric functionality for
Lorentz transforms.  It deals with formal Lorentz boosts and converts
between three-velocities and four-velocities.  Computational support
for the nonassociative and noncommutative gyrogroup structure of
relativistic three-velocity addition is included.  Some functionality
for relativistic transformation of tensors of order two such as the
stress-energy tensor is given, with examples.  In the package, the
speed of light is one by default, but is user-settable and the
classical limit is recovered by setting $c=\infty$.  Both passive and
active transforms are implemented.  An extensive heuristic vignette
detailing package idiom is included.  There does not seem to be a
known relativistic generalization of the classical distributive law
$r\left({\bf u} + {\bf v}\right)=r{\bf u} + r{\bf v}$ [@ungar1997].
Ungar states that ``It is hoped that one day a gyrodistributive law
$\ldots$ will be discovered.  If exists, it is expected to be the
standard distributive law relaxed by Thomas gyration in some
unexpected way''.  The package is used to execute a systematic sweep
through possible distributive laws consistent with Ungar's suggestion,
unfortunately without success.

# References
