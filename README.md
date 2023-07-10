Discovering Active Subpaces for High Dimensional Computer Models
(Examples and R Code)
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

**Note:** The rendered quarto document `Examples_Walkthrough` is a good
place to start. It provides easy-to-follow implementation details for
Sections 4.1-4.2 in the manuscript (linked here when available).
Additional scripts are included to reproduce the results included in
Sections 4.1-4.2 as well as some of the results in the Supplemental
materials.

This repository contains several R scripts to reproduce the examples
found in the (submitted for review) manuscript *Active Subspaces for
High Dimensional Computer Models*. To run these scripts, you will need
to install the `concordance` and `GBASS` packages (other packages from
CRAN may be needed, see `R/setup.R` for details) from public github
repos using the commands.

``` r
# install.packages("devtools")
devtools::install_github("knrumsey/concordance")
devtools::install_github("knrumsey/GBASS") # only needed for supplemental materials
```

The scripts included in this repository include

- `Examples_Walkthrough.qmd`
- `R/A_simple_polynomial.R` (Section 4.1)
- `R/B_linear_constraint.R` (Section 4.1.1)
- `R/C_low_dim_structure.R` (Section 4.2)
- `R/D_polynomial_outliers.R` (Supplemental materials)
- `R/E_polynomial_quantile.R` (Supplemental materials)
- `R/F_activity_posterior.R` (Supplemental materials)
- `R/G_Interaction_Order.R` (Supplemental materials)

## Copyright Notice

© *2023. Triad National Security, LLC. All rights reserved.*

*This program was produced under U.S. Government contract
89233218CNA000001 for Los Alamos National Laboratory (LANL), which is
operated by Triad National Security, LLC for the U.S. Department of
Energy/National Nuclear Security Administration. All rights in the
program are reserved by Triad National Security, LLC, and the U.S.
Department of Energy/National Nuclear Security Administration. The
Government is granted for itself and others acting on its behalf a
nonexclusive, paid-up, irrevocable worldwide license in this material to
reproduce, prepare derivative works, distribute copies to the public,
perform publicly and display publicly, and to permit others to do so.*
