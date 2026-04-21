# TidySynt

This is an R package used to produce make synteny plots using paf files. To see how to use this package, you can use the html tutorial that explains how to make the paf file using fasta files (for example with reference genomes). The idea is to run `minimap2` [(Li H, 2018)](https://doi.org/10.1093/bioinformatics/bty191) to produce paf files. In these files, we have some coordinates of sequences from one genome (the query) that map on another genome used as reference (the target). We use this format to produce some dotplots and synteny analyses and provide a package of functions that use the tidy format [(Wickham et al., 2019)](https://doi.org/10.21105/joss.01686).

The traced plots are ggplot2 objects so you can modify everything in the way that you like (change the colours, remove the contours, ...)

# Install the package

Open an R console and type
```{r}
if (!require(devtools)) install.packages("devtools")
devtools::install_github("PAJOT-Basile/Tidysynt")
```

# Quick start

Download and read the tutorial.

# Example of what can be done

![All_chromosomes](./img/All_chromosomes.png)

# References
  - [Li, H. (2018). Minimap2: pairwise alignment for nucleotide sequences. Bioinformatics, 34:3094-3100. doi:10.1093/bioinformatics/bty191](doi:10.1093/bioinformatics/bty191)
  - [Wickham et al., (2019). Welcome to the Tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686](https://doi.org/10.21105/joss.01686)
