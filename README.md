# **LFMC Database**

## Description

Package **LFMC** is designed to assist the creation, management, and
analysis of the live fuel moisture content (LFMC) database of the
*Servei de Prevenció d’Incendis Forestals* (SPIF) of the Catalan
administration. The database is the outcome of a biweekly monitoring of
five Mediterranean plant species in nine Catalonian study sites since
1998 (see the [data paper](https://annforsci.biomedcentral.com/articles/10.1007/s13595-021-01057-0) for details).

## Database design and structure  

A relational database was designed to store LFMC data in a reliable, structured, and dynamic format, ensuring long-term integrity. Additionally, this approach provides a fast and flexible access to data, while maintaining the database in a consistent state. The logical relational model for LFMC database is shown below.

![](vignettes/SPIF_LogicalSchema.png)

## Package installation

You can download and install the latest stable versions of the R package
from GitHub as follows (required package `devtools` should be
installed/updated first):

``` r
devtools::install_github("spif-ctfc/LFMC")
```

