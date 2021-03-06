
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cupersonnel

The goal of cupersonnel is to facilitate analysis of University of
Colorado personnel data.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jayhesselberth/cupersonnel")
```

## Example

Here’s the disibution of CU salaries across campuses:

``` r
library(cupersonnel)
library(tidyverse)
library(cowplot)
library(ggridges)

cu_personnel %>%
  ggplot(aes(x = log10(funding + 1), y = campus, fill = campus)) +
  geom_density_ridges(alpha = 0.4) + 
  scale_fill_brewer(palette = "Set1")
```

<img src="man/figures/README-campus_salaries-1.png" width="100%" />
