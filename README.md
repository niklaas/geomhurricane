[![Build Status](https://travis-ci.org/niklaas/geomhurricane.svg?branch=master)](https://travis-ci.org/niklaas/geomhurricane)

# geomhurricane

The goal of geomhurricane is to provide a geom for `ggplot2` that plots wind
roses (for an example see [Wikipedia][wiki]). It provides a solution to the
final exam of [a Coursera course][coursera].

There is a vignette that shortly describes how to use `geom_hurricane` and
provides an answer to the exam.

[wiki]: https://en.wikipedia.org/wiki/Wind_rose
[coursera]: https://www.coursera.org/learn/r-data-visualization

## Possible improvements

- [ ] Support `units`
    - At this stage, unit conversion from nautical miles to meters (needed for
      `geosphere::destPoint`) is hard coded. Wind speeds are provided in knots
      and the legend of the plot is edited manually to reflect that.
    - Unit `nautical_mile` and `knot` already exist and are usable with package
      `units`. But `ggplot2` doesn't support scales of type `units` in its base
      implementation.
    - Package `ggforce` enhances `ggplot2` to support `units` but I haven't
      figured out how to import this feature into this package yet.
- [ ] Plotting multiple storms (see TODOs in R/geom_hurricane)
    - [ ] same storm, different date-times
    - [ ] different storms, different date_times
- [ ] Plotting multiple storms (see TODOs in R/geom_hurricane)
    - [ ] same storm, different date-times
    - [ ] different storms, different date_times
