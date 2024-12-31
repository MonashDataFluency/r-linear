# r-linear

Workshop material on the use of linear models in R.

## Building the material

To build first install R packages as below, then type `make` on the command line.

```
# Install some CRAN packages:
install.packages(c(
    "rmarkdown", "plot3D", "gifski",
    "tidyverse", "multcomp", "emmeans", 
    "lme4", "lmerTest", "pbkrtest", "BiocManager"))

# Install some Bioconductor packages:
BiocManager::install(c("limma","edgeR","topconfects"))
```

