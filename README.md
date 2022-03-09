# An R interface for the DBpedia Databus

R package **r2databus**, version **0.1.0.9003**

[![CRAN status](https://www.r-pkg.org/badges/version/r2databus)](https://cran.r-project.org/package=r2databus)  [![R build status](https://github.com/giannou/r2databus/workflows/check/badge.svg)](https://github.com/giannou/r2databus/actions) [![codecov](https://codecov.io/gh/giannou/r2databus/branch/master/graph/badge.svg)](https://app.codecov.io/gh/giannou/r2databus) 

## Purpose and Functionality

Provides tools for uploading, downloading, and annotating data to 
    the DBpedia Databus.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("r2databus")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Anastasis Giannousakis <giannou@pik-potsdam.de>.

## Citation

To cite package **r2databus** in publications use:

Giannousakis A (2022). _r2databus: An R interface for the DBpedia Databus_. R package version 0.1.0.9003.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {r2databus: An R interface for the DBpedia Databus},
  author = {Anastasis Giannousakis},
  year = {2022},
  note = {R package version 0.1.0.9003},
}
```
