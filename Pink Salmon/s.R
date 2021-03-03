### Individual Based Models In R
# from Brad Duthie, https://bradduthie.github.io/blog/individual-based-models-in-r/

## load libraries

library(tidyverse); library(patchwork)

#####################################################################################
# Section 2:Getting started with IBMs in R

inds <-  array(data = 0, dim = c(5,3))
colnames(inds) <- c("characteristic_1", "characteristic_2", "characteristic_3")
rownames(inds) <- c("ind_1", "ind_2", "ind_3", ind_1)