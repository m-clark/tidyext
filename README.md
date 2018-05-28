tidyext
=======

<!-- <img src="man/img/lh_hex.png" align="right" width = 360/> -->
<!-- [![Build Status](https://travis-ci.org/m-clark/lazerhawk.svg?branch=master)](https://travis-ci.org/m-clark/lazerhawk) -->
<!-- [![codecov](https://codecov.io/gh/m-clark/lazerhawk/branch/master/graph/badge.svg)](https://codecov.io/gh/m-clark/lazerhawk) -->
<a href="https://github.com/m-clark/tidyext" alt="Miscellaneous Shenanigans"> <img src="https://img.shields.io/badge/miscellaneous_shenanigans-constant-ff5500.svg?colorA=00aaff&longCache=true&style=for-the-badge"  width=27.5%/></a>

This package takes some data processing functions originally found in [lazerhawk](https://github.com/m-clark/lazerhawk), extending the functionality of the tidyverse. For folks that do a lot of data processing in that world, these make some of those very common tasks a bit easier, and with an eye toward clean presentation and visualization.

As these functions are more universally useful, especially to my colleagues and friends who use R, putting them as their own thing will perhaps make the functionality easier to use for them. Also, as everything is programmed with the tidyverse, these functions are easily customizable. The goal is more or less for this to depend on nothing one wouldn't have already with base R and the tidyverse package loaded.

Installation
------------

The usual GitHub installation. **devtools** package required.

``` r
devtools::install_github('m-clark/tidyext')
```

Functions
---------

### cat\_by

A quick summarize for categorical variables, possibly with dplyr::group\_by, that provides frequencies and percentages of categories.

### combn\_2\_col

Takes a column with multiple entries per cell and creates indicator columns of all possible combinations of the cell values up to m combinations.

### create\_prediction\_data

Straightforward way to quickly create data to make model predictions.

### describe\_all

A summary function for mixed data types that provides the information I usually want.

### num\_by

A quick summarize, possibly with dplyr::group\_by, that provides things like mean, sd, etc.

### num\_summary

A little better than summary.

### onehot

A function for one-hot encoding with a few helpful options.

### sum\_NA, sum\_NaN, sum\_blank

Understand your nothingness.
