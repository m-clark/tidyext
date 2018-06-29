[![Build
Status](https://travis-ci.org/m-clark/tidyext.svg?branch=master)](https://travis-ci.org/m-clark/tidyext)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/m-clark/tidyext?branch=master&svg=true)](https://ci.appveyor.com/project/m-clark/tidyext)
[![codecov](https://codecov.io/gh/m-clark/tidyext/branch/master/graph/badge.svg)](https://codecov.io/gh/m-clark/tidyext)
<a href="https://github.com/m-clark/tidyext" alt="Miscellaneous Shenanigans">
<img src="https://img.shields.io/badge/miscellaneous_shenanigans-constant-ff5500.svg?colorA=00aaff&longCache=true&style=for-the-badge"  width=27.5%/></a>

# tidyext

<img src="man/figures/tidyext_hex.png" align="right" width = 200/>

<br>

### Overview

Extensions and extras for tidy processing. This package provides some
data processing and summarizing functions that would commonly be useful
in the tidyverse. For folks that do a lot of data processing in that
world, these make a handful of some very common tasks a bit easier, and
with an eye toward eventual tidy/clean presentation and visualization
with tools like
[kableExtra](https://haozhu233.github.io/kableExtra/awesome_table_in_html.html)
and [ggplot2](http://ggplot2.tidyverse.org/).

As these functions are more universally useful, especially to my
colleagues and friends who use R, putting them as their own package with
few dependencies will perhaps make it easier to use for them. The goal
is more or less for this to depend on nothing one wouldn’t have already
with base R and the tidyverse package loaded. Also, as all the functions
use the tidyverse functionality, they are easily customizable.

### Installation

To install from GitHub the <span class="pack">devtools</span> package is
required.

``` r
devtools::install_github('m-clark/tidyext')
```

Note that this package more or less assumes your are working within the
<span class="pack">tidyverse</span>, especially
<span class="pack">dplyr</span>. As such you should have the
<span class="pack">tidyverse</span> packages installed.

### Functions

  - <span class="func">cat\_by</span>: A quick summarize for categorical
    variables, possibly with `dplyr::group_by`, that provides
    frequencies and percentages of categories, ready for publishing
    tables or plotting.

  - <span class="func">combn\_2\_col</span>: Takes a column with
    multiple entries per cell and creates indicator columns of all
    possible combinations of the cell values up to m combinations.

  - <span class="func">create\_prediction\_data</span>: Straightforward
    way to quickly create data to make model predictions.

  - <span class="func">describe\_all</span>: A summary function for
    mixed data types that provides the information I usually want. Saves
    one from doing a `group_by %>% summarize` operation to create
    multiple results for multiple types of variables. Has corresponding
    <span class="func">describe\_all\_num</span> and
    <span class="func">describe\_all\_cat</span> for numeric-only and
    categorical-only data respectively.

  - <span class="func">gather\_multi</span>: Gather multiple sets of
    variables.

  - <span class="func">num\_by</span>: A quick summarize, possibly with
    `dplyr::group_by`, that provides things like mean, sd, etc. See
    <span class="func">num\_summary</span>.

  - <span class="func">num\_summary</span>: A little better than the
    base R summary, gives the info one typically wants as well as
    options for rounding and other statistics.

  - <span class="func">onehot</span>: A function for one-hot encoding
    with a few helpful options for dealing with missing data, using
    sparse matrices, and more.

  - <span class="func">pre\_process</span>: Easily pre-process a data
    set with common operations like standardization, logging, etc.

  - <span class="func">spread2</span>: The tidyr spread without the
    duplicate row id problem.

  - <span class="func">sum\_NA</span>,
    <span class="func">sum\_NaN</span>,
    <span class="func">sum\_blank</span>: Understand your nothingness.

<br>

### Version history

  - 0.2.0 Website
  - 0.1.3 Added gather\_multi
  - 0.1.2 Added spread2
  - 0.1.1 Added pre\_process
  - 0.1.0 Initial release
