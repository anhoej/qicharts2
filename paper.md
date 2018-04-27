---
title: 'qicharts2: Quality Improvement Charts for R'
tags:
  - Statistical process control
  - Continuous quality improvement
authors:
- name: Jacob Anhøj
  orcid:  0000-0002-7701-1774
  affiliation: "1"
affiliations:
- name: Rigshospitalet, University of Copenhagen, Denmark
  index: 1
date: 18 April 2018
bibliography: paper.bib
---

# Summary

`qicharts2` is an R package for making statistical process control (SPC) charts for continuous quality improvement.

![Figure 1: Control chart of hospital infections showing random variation](paper.png)

SPC charts are point-and-line graphs showing a measure over time and employing statistical tests for identification of non-random variation, e.g. quality improvement or degradation. Central to SPC is the understanding of process variation over time.

The purpose of analysing process data is to know when a change occurs so that we can take appropriate action. However, numbers may change even if the process stays the same (and vice versa). So how do we distinguish changes in numbers that represent change of the underlying process from those that are essentially noise?

[Walther A. Shewhart](http://en.wikipedia.org/wiki/Walter_A._Shewhart), who founded SPC, described two types of variation, *common cause variation* and *special cause variation*:

**Common cause variation**

* is also called natural/random variation or noise,
* is present in all processes,
* is caused by phenomena that are always present within the system,
* makes the process predictable within limits.

**Special cause variation**

* is also called non-random variation or signal,
* is present in some processes,
* is caused by phenomena that are not normally present in the system,
* makes the process unpredictable.

The overall purpose of SPC charts is to tell the two types of variation apart.

Special cause variation in the form of minor to moderate persistent shifts in data over time is identified by the Anhoej rules for unusually long runs and unusually few crossings [@anhøj2014; @anhøj2015].

Special cause variation in the form of larger, possibly transient, shifts is identified by Shewhart's 3-sigma rule [@mohammed2008] as coloured data points outside the control limits.

The control chart shown in Figure 1 displays common cause variation. This means that the process is stable and predictable, and that in the future -- if nothing changes -- we should expect on average 19 infections per month, and that as few as 6 and as many as 32 would be within the expected range.

The main function of `qicharts2`, `qic()`, has a simple interface with only one mandatory argument (the numbers to plot). Many more optional arguments allow for customisation of the plot, including selection of control chart type, one and two dimensional facetting, and splitting of charts into pre and post intervention periods.

With `qicharts2` comes a vignette, Get started [@vignette], that explains the theory behind SPC and typical uses of `qic()`.

# References
