# geomhurricane

The goal of geomhurricane is to provide a geom for `ggplot2` that plots wind
roses (for an example see [Wikipedia][wiki]). It provides a solution to the
final exam of [a Coursera course][coursera].

[wiki]: https://en.wikipedia.org/wiki/Wind_rose
[coursera]: https://www.coursera.org/learn/r-data-visualization

## Possible improvements

[_] Support `units`
    - At this stage, unit conversion from nautical miles to meters (needed for
      `geosphere::destPoint`) is hard coded. Wind speeds are provided in knots
      and the legend of the plot is edited manually to reflect that.
    - Unit `nautical_mile` and `knot` already exist and are usable with package
      `units`. But `ggplot2` doesn't support scales of type `units` in its base
      implementation.
    - Package `ggforce` enhances `ggplot2` to support `units` but I haven't
      figured out how to import this feature into this package yet.
[_] Plotting multiple storms (see TODOs in R/geom_hurricane)
    [_] same storm, different date-times
    [_] different storms, different date_times
