R scripts for ‘Discovering Active Subpaces for High Dimensional Computer
Models’
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

This repository contains several R scripts to reproduce the examples
found in the (submitted for review) manuscript *Active Subspaces for
High Dimensional Computer Models*.

To run these scripts, you will need to install the `concordance` and
`GBASS` packages (other packages from CRAN may be neeed, see `R/setup.R`
for details) from public github repos using the commands.

``` r
# install.packages("devtools")
devtools::install_github("knrumsey/concordance")
devtools::install_github("knrumsey/GBASS")
```

A link to the manuscript will be included when available. A Quarto
document (and the rendered pdf) titled `Examples_Walkthrough` includes a
demonstration of the examples from sections 4.1 and 4.2 in the
manuscript. Additional scripts are included to reproduce the results
included in Sections 4.1-4.2 as well as some of the results in the
Supplemental materials. The scripts included in this repository include

- `A_simple_polynomial.R` (Section 4.1)
- `B_linear_constraint.R` (Section 4.1.1)
- `C_low_dim_structure.R` (Section 4.2)
- `D_polynomial_outliers.R` (Supplemental materials)
- `E_polynomial_quantile.R` (Supplemental materials)

# References
