[![Build
Status](https://travis-ci.org/m-clark/tidyext.svg?branch=master)](https://travis-ci.org/m-clark/tidyext)
[![codecov](https://codecov.io/gh/m-clark/tidyext/branch/master/graph/badge.svg)](https://codecov.io/gh/m-clark/tidyext)
<a href="https://github.com/m-clark/tidyext" alt="Miscellaneous Shenanigans">
<img src="https://img.shields.io/badge/miscellaneous_shenanigans-constant-ff5500.svg?colorA=00aaff&longCache=true&style=for-the-badge"  width=27.5%/></a>

tidyext
=======

<img src="man/figures/tidyext_hex.png" align="right" width = 200/>

<br>

This package takes some data processing functions originally found in
[lazerhawk](https://github.com/m-clark/lazerhawk), extending the
functionality of the tidyverse. For folks that do a lot of data
processing in that world, these make a handful of those very common
tasks a bit easier, and with an eye toward eventual tidy/clean
presentation and visualization with tools like
[kableExtra](https://haozhu233.github.io/kableExtra/awesome_table_in_html.html)
and [ggplot2](http://ggplot2.tidyverse.org/).

As these functions are more universally useful, especially to my
colleagues and friends who use R, putting them as their own package with
few dependencies will perhaps make it easier to use for them. The goal
is more or less for this to depend on nothing one wouldn’t have already
with base R and the tidyverse package loaded. Also, as all the functions
use the tidyverse functionality, they are easily customizable.

### Installation

The usual GitHub installation. **devtools** package required.

``` r
devtools::install_github('m-clark/tidyext')
```

### Functions

#### cat\_by

A quick summarize for categorical variables, possibly with
dplyr::group\_by, that provides frequencies and percentages of
categories, ready for publishing tables or plotting.

#### combn\_2\_col

Takes a column with multiple entries per cell and creates indicator
columns of all possible combinations of the cell values up to m
combinations.

#### create\_prediction\_data

Straightforward way to quickly create data to make model predictions.

#### describe\_all

A summary function for mixed data types that provides the information I
usually want. Saves one from doing a group\_by %&gt;% summarize
operation to create multiple results for multiple types of variables.
Has corresponding describe\_all\_num and describe\_all\_cat for
numeric-only and categorical-only data respectively.

#### num\_by

A quick summarize, possibly with dplyr::group\_by, that provides things
like mean, sd, etc. See num\_summary.

#### num\_summary

A little better than the base R summary, gives the info one typically
wants as well as options for rounding and other statistics.

#### onehot

A function for one-hot encoding with a few helpful options for dealing
with missing data, using sparse matrices, and more.

#### pre\_process

Easily pre-process a data set with common operations like
standardization, logging, etc.

#### spread2

The tidyr spread without the duplicate row id problem.

#### sum\_NA, sum\_NaN, sum\_blank

Understand your nothingness.

<br>

### Version history

-   0.1.2 Added spread2
-   0.1.1 Added pre\_process
-   0.1.0 Initial release
