
## `agricolae`: Diallel Analysis with R

###### Version : [1.3.3](https://myaseen208.github.io/agricolae/); Copyright (C) 2019-2020: [UAF](http://uaf.edu.pk//); License: [GPL-2|GPL-3](https://www.r-project.org/Licenses/)

##### *Muhammad Yaseen<sup>1</sup>, and Kent M. Eskridge<sup>2</sup>*

1.  Department of Mathematics and Statistics, University of Agriculture
    Faisalabad, Pakistan.
2.  Department of Statistics, University of Nebraska Lincoln, NE, USA.

-----

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-last-release/agricolae)](https://cran.r-project.org/package=agricolae)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/agricolae?color=green)](https://CRAN.R-project.org/package=agricolae)
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-0.2.3.3-orange.svg)](https://github.com/myaseen208/agricolae) -->

<!-- [![GitHub Download Count](https://github-basic-badges.herokuapp.com/downloads/myaseen208/agricolae/total.svg)] -->

[![Project Status:
WIP](http://www.repostatus.org/badges/latest/inactive.svg)](http://www.repostatus.org/#inactive)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--02--14-yellowgreen.svg)](https://github.com/myaseen208/agricolae)
[![Rdoc](http://www.rdocumentation.org/badges/version/agricolae)](http://www.rdocumentation.org/packages/agricolae)

-----

## Description

Original idea was presented in the thesis “A statistical analysis tool
for agricultural research” to obtain the degree of Master on science,
National Engineering University (UNI), Lima-Peru. Some experimental data
for the examples come from the CIP and others research. Agricolae offers
extensive functionality on experimental design especially for
agricultural and plant breeding experiments, which can also be useful
for other purposes. It supports planning of lattice, Alpha, Cyclic,
Complete Block, Latin Square, Graeco-Latin Squares, augmented block,
factorial, split and strip plot designs. There are also various analysis
facilities for experimental data, e.g. treatment comparison procedures
and several non-parametric tests comparison, biodiversity indexes and
consensus cluster.

## Installation

The package can be installed from CRAN as follows:

``` r
install.packages("agricolae", dependencies = TRUE)
```

The development version can be installed from github as follows:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("myaseen208/agricolae")
```

## Detailed tutorial

For a detailed tutorial (vignette) on how to used this package type:

``` r
browseVignettes(package = "agricolae")
```

The vignette for the latest version is also available
[online](https://myaseen208.github.io/agricolae/articles/Introagricolae.html).

## What’s new

To know whats new in this version type:

``` r
news(package = "agricolae")
```

## Links

[CRAN page](https://cran.r-project.org/package=agricolae)

[Github page](https://github.com/myaseen208/agricolae)

[Documentation website](https://myaseen208.github.io/agricolae/)

## Citing `agricolae`

To cite the methods in the package
    use:

``` r
citation("agricolae")
```

    Warning in citation("agricolae"): no date field in DESCRIPTION file of package
    'agricolae'
    Warning in citation("agricolae"): could not determine year for 'agricolae' from
    package DESCRIPTION file
    
    To cite package 'agricolae' in publications use:
    
      Felipe de Mendiburu (NA). agricolae: Statistical Procedures for
      Agricultural Research. R package version 1.3-3.
      http://tarwi.lamolina.edu.pe/~fmendiburu
    
    A BibTeX entry for LaTeX users is
    
      @Manual{,
        title = {agricolae: Statistical Procedures for Agricultural Research},
        author = {Felipe {de Mendiburu}},
        note = {R package version 1.3-3},
        url = {http://tarwi.lamolina.edu.pe/~fmendiburu},
      }
    
    ATTENTION: This citation information has been auto-generated from the
    package DESCRIPTION file and may need manual editing, see
    'help("citation")'.
