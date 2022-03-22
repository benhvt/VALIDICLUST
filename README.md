# PCVI
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/benhvt/PCVI/workflows/R-CMD-check/badge.svg)](https://github.com/benhvt/PCVI/actions)
  <!-- badges: end -->

# Overview 

`PCVI` is a package to ensure valid inference after data clustering. The 3 main functions of the package are :

* `test_selective_inference()`  adpated from the work of Gao et al. [[1]](#1) concerning selective inference for clustering 

* `merge_selective_inference()` for the merging selective test. With this method, all the adjacent p-values are merging using harmonic mean 

* `test_multimod()` using the DipTest [[2]](#2) to study the separation of two estimated clusters on a given variable 

# Installation

Install the development version from GitHub

```r 
remotes::install_github("benhvt/PCVI")
```

# References
<a id="1">[1]</a> 
Gao, L. L., Bien, J., & Witten, D. (2020). Selective inference for hierarchical clustering. arXiv preprint arXiv:2012.02936.

<a id="2">[2]</a>
HARTIGAN, John A. et HARTIGAN, Pamela M. The dip test of unimodality. The annals of Statistics, 1985, p. 70-84.
