# Quality improvement charts for R

Functions for making run charts, Shewhart control charts and Pareto charts for continual quality improvement. Included control charts are: I, MR, Xbar, S, T, C, U, U', P, P', and G charts. 

Non-random variation in the form of minor to moderate persistens shifts in data over time is identified by the Anhoej rules for unusually long runs and unusually few crossing.

Non-random variation in the form of larger, possibly transient, shifts in data is identified by Shewhart's 3-sigma rule.

## Exported functions

* `qic()` Run and control charts.

* `summary.qic()` Summary of qic object.

* `paretochart()` Pareto charts.

## Included datasets

* **hospital_infections** Number of hospital acquired bacteremia, Clostridium difficile infections, and urinary tract infections in six hospitals.

* **cabg** Individual coronary artery bypass graft operations.

* **cdi** Hospital acquired Clostridium difficile infections (CDI) before and after an intervention to reduce the risk of CDI.

* **nhs_accidents** Number of attendances to major accident and emergency hospital departments in the NHS that were seen within 4 hours of arrival over twenty weeks.

* **gtt** Adverse events during hospitalisation found by the Global Trigger Tool.

## Instructions
    
* Install from github: `devtools::install_github('anhoej/qicharts2', build_vignettes = TRUE)`.

* Read documentation: `?qic`.

* Run examples: `example('qic')`, `example('paretochart')`.

* Read vignette: `vignette('qicharts')`.

* Report issues and suggest improvements on https://github.com/anhoej/qicharts2.
