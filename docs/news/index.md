# Changelog

## qicharts2 0.8.1.9999

- Allow missing target in bchart().

## qicharts2 0.8.1

CRAN release: 2025-06-23

- Prepare for ggplot2 version 4 (size -\> linewidth, aes\_ -\> aes).

## qicharts2 0.8.0

CRAN release: 2025-05-15

- Add option qic.mrscreened.
- Add I prime chart (ip).

## qicharts2 0.7.5

CRAN release: 2024-05-09

- Build for R 4.4.0
- Minor fixes to docs
- Change target line to dotted

## qicharts2 0.7.4

CRAN release: 2023-08-21

- Fix error
- Screen mr during prime adjustment for pp and up charts (thans to
  andrjohns).
- Prevent conversion of missing value to 0 in C chart (thanks to
  NathanProudlove).
- Replace the old travis build check with a Github Actions R-CMD-CHECK
  (thanks to andrjohns).
- Add option to show 2 sigma limits (thanks to andrjohns).
- Increased default point size.
- Change signal colour.

## qicharts2 0.7.2

CRAN release: 2021-07-08

- Fixed bug in plotting categorical (character) subgroups.
- Prime charts (pp and up) now handle categorical subgroups (funnel
  plots).
- qic() gained y.percent.accuracy argument.
- Add return data param to qic() (thanks to rjake).
- Fix DOI URLs.

## qicharts2 0.7.1

CRAN release: 2020-10-28

- qic() gained EXPERIMENTAL method argument.
- Fixed broken link in vignette.
- Fixed limits in S charts using freeze argument (thanks to dirkse).
- Fixed part argument with subgrouped data (thanks to Nathan Proudlove),

## qicharts2 0.6.1

CRAN release: 2020-03-15

- Fixing a warning caused by a recent update to geom_ribbon()
- Fixed missing options when package not loaded.
- New EXPERIMENTAL function, bchart() for Bernoulli CUSUM charts for
  rare events.
- The part argument now accepts a character vector identifying and
  labelling chart parts.

## qicharts2 0.6.0

CRAN release: 2019-03-16

- Allow NULL as title argument in paretochart (thanks to Andrew Hill).
- Fix c4() for large n (thanks to pwildenhain for the bug report and
  Tore Wenzel-Larsen for the fix).
- Hide line labels for Inf values.
- Fix baseline centre line in G charts (when using freeze argument).
- Fix error when x.period argument is NULL (thanks to lenakba).

## qicharts2 0.5.1

CRAN release: 2018-07-10

- Fix missing target line.
- Use exact values for control chart constants A3, B3, and B4 (thanks to
  Tore Wenzel-Larsen).
- Add x.period argument specifying interval cut points for aggregating y
  values by week, month, etc. (thanks to pwildenhain).
- P charts default to percentage on Y axis (thanks to pwildehain).
- Add qic.linecol, qic.signalcol, and qic.targetcol to options to allow
  for custom colours.
- Add qic.clshade to options to allow for choosing between lines and
  shaded area for control limits.
- Fix typos in vignette (thanks to rmadillo).
- Add source to cabg and gtt datasets.

## qicharts2 0.4.0

CRAN release: 2018-03-03

- Fix quoted variables.
- Darker CL background.
- Add point.size argument.
- Improved notes and parts labels.

## qicharts2 0.3.0

CRAN release: 2018-02-03

- Skip more tests that generate noLD issues on CRAN.
- Allow NULL as title argument to suppress chart title.
- Add strip.horizontal argument to qic().

## qicharts2 0.2.3

CRAN release: 2017-12-16

- Skip tests that generate noLD issues on CRAN.

## qicharts2 0.2.2

- Fix time zone warnings on Windows environment.

## qicharts2 0.2.1

CRAN release: 2017-12-08

- Fix x axis error when dates crosses summer/winter time.

## qicharts2 0.2.0

CRAN release: 2017-11-19

- Import dplyr \>= 0.7.0
- Fix calculation of control limits for xbar chart in qic() with freeze
  or exclude argument.
- C chart ignores agg.fun argument.

## qicharts2 0.1.1

CRAN release: 2017-08-28

- First release.
