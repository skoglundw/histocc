# histocc

<!-- badges: start -->
<!-- badges: end -->

## Overview

The `histocc` R package provides tools for standardizing and classifying historical Swedish occupational titles from parish records, census data, and other historical sources (circa 1750-1950).

## Features

- **Standardization**: Clean and standardize messy historical occupation titles using fuzzy matching
- **HISCO Classification**: Code occupations using the Historical International Standard Classification of Occupations
- **HISCLASS**: Apply social class schemes with sophisticated status-based adjustments
- **Sector Classification**: Categorize occupations by economic sector, subsector, and worker type

## Installation

You can install the development version of histocc from GitHub:

```r
# install.packages("devtools")
devtools::install_github("skoglundw/histocc")
```

## Quick Start

```r
library(histocc)

# Example data with messy occupation titles
df <- data.frame(
  name = c("Anders Andersson", "Per Persson", "Lars Larsson"),
  occupation = c("smed", "snickare mästare", "sold."),
  year = c(1850, 1875, 1900)
)

# Standardize and classify in one step
df_coded <- occ_hisco(df, "occupation", 
                      standardize_occupations = TRUE,
                      hisclass = TRUE, 
                      status = TRUE)

# Add sector classifications
df_coded <- occ_sector(df_coded, "standard_occupation", 
                       label = TRUE)
```

## Main Functions

### `standardize_occupation()`
Standardizes occupation titles using a three-step matching process (direct match → exact match → fuzzy match).

### `occ_hisco()`
Codes occupations using HISCO and optionally adds:
- HISCO descriptions
- Occupational status codes  
- HISCLASS-12 social class codes
- Income scores

### `occ_sector()`
Classifies occupations by:
- Economic sector (primary/secondary/tertiary)
- Subsector
- Worker type


## Citation

If you use this package in your research, please cite:

```
Skoglund, W. (2025). histocc: Classification of Historical Swedish Occupations. 
R package version 0.1.0. https://github.com/skoglundw/histocc
```

## References
- van Leeuwen, M.H.D. & Maas, I. (2011). *HISCLASS: A Historical International Social Class Scheme*. Leuven University Press.
- Bengtsson, Erik, Svante Prado, and Jakob Molinder. 2021. “The Swedish Transition to Equality: Income Inequality with New Micro Data, 1870-1970.” Machine-readable data file.
## Contact

William Skoglund  
Department of Economic History, Uppsala University  
Email: william.skoglund@ekhist.uu.se

## License

MIT License - see LICENSE file for details
