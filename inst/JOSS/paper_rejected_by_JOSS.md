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
- Wigner rotation
- Nonassociative operation
- gyrogroup
- Thomas precession
affiliations:
- index: 1
  name: Auckland University of Technology
---

# Introduction: the Lorentz transform in special relativity

In special relativity, the Lorentz transforms supercede their
classical equivalent, the Galilean transforms [@goldstein1980].
Lorentz transforms operate on four-vectors such as the four-velocity
or four-potential and are usually operationalised as multiplication by
a $4\times 4$ matrix.  A Lorentz transform takes the components of an
arbitrary four-vector as observed in one coordinate system and returns
the components observed in another system which is moving at constant
velocity with respect to the first.

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

## Statement of need

However, there does not appear to be an R package designed for
systematic numerical investigation of the Lorentz group.  Here, I
present `lorentz`, a consistent and documented suite of software that
allows the user to manipulate Lorentz boosts---considered as members
of $O(3,1)$ or $SO(3,1)$---and also facilitates investigation into the
nonassociative and noncommutative "gyrogroup" structure of
three-velocities [@ungar1997].  The package allows the user to employ
efficient and natural vectorised R idiom in the context of
relativistic kinematics.

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
detailing package idiom is included.

# Research application

There does not seem to be a known relativistic generalization of the
classical distributive law $r\left({\bf u} + {\bf v}\right)=r{\bf u} +
r{\bf v}$.  Ungar [-@ungar1997] states that ``It is hoped that one day
a gyrodistributive law \ldots will be discovered.  If exists, it is
expected to be the standard distributive law relaxed by Thomas
gyration in some unexpected way''.  The package is used to execute a
systematic sweep through a total of $2^{15}\cdot 3\cdot 7=688128$
possible distributive laws consistent with Ungar's suggestion,
unfortunately without success.

# References
