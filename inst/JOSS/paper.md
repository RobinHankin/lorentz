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

# Summary

In special relativity, the Lorentz transforms supercede their
classical equivalent, the Galilean transforms [@goldstein1980].
Lorentz transforms operate on four-vectors such as the four-velocity
or four-potential and are usually operationalised as multiplication by
a $4\times 4$ matrix.  A Lorentz transform takes the components of an
arbitrary four-vector as observed in one coordinate system and returns
the components observed in another system which is moving at constant
velocity with respect to the first.

There are a few existing software tools for working with Lorentz
transforms, mostly developed in an educational context.  Early work
would include that of Horwitz et al. [-@horwitz1992], who describe
``relLab``, a system for building a range of *gendanken* experiments
in an interactive graphical environment.  The author asserts that it
runs on "any Macintosh computer with one megabyte of RAM or more" but
it is not clear whether the software is still available.  More modern
contributions would include the ``OpenRelativity`` toolkit
[@sherin2016] which simulates the effects of special relativity in the
``Unity`` game engine.

The ``lorentz`` package provides ``R``-centric functionality for
Lorentz transforms.  It deals with formal Lorentz boosts, converts
between three-velocities and four-velocities, and provides
computational support for the gyrogroup structure of relativistic
three-velocity addition.  The speed of light is one by default, but is
user-settable and the classical limit is recovered by setting
$c=\infty$.  Both passive and active transforms are implemented.

An extensive heuristic vignette detailing package idiom is included.
The package is used to execute a formal sweep through many
possibilities, searching for a distributive law following an
observation of Ungar [-@ungar2006].

# References
