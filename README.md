
# farsFiki

  <!-- badges: start -->
  [![Build Status](https://travis-ci.com/fsigal/farsFiki.svg?branch=main)](https://travis-ci.com/fsigal/farsFiki)
  <!-- badges: end -->

The goal of farsFiki is to import csv files with accidents data, summarize accidents information and create maps with this data.

## Installation

You can install the released version of farsFiki from [GitHub](https://github.com/fsigal/farsFiki) with:

``` r
install_github("fsigal/farsFiki")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(farsFiki)
fars_read(accident_2010.csv.bz2)
make_filename(2010)
fars_read_years(2010:2019)
fars_summarize_years(2010:2019)
fars_map_state(17,2010)
```
