# KBminmaxpoly
A package for calculating confidence bands

This is the uploaded version of my Bachelorthesis. This R package calculates confidence bands.

There are the following main function:

1. KB.R which calculates the critical constant of a confidence band on the whole of R
2. KB.R.pruef
3. KB.minmax
4. KB.minmax.poly
5. KB.minmax.poly.fast

-> this functions can be found in R/crit-par-functions.R

Furthermore, there are three functions to calculate the confidence bands once the critical constant is calculated:

6. plot.KB
7. plot.KB.vergl
8. plot.KB.pruef

There are a bunch of supplementary functions like estimation functions in estimation-functions.R. In addition, there are support functions in support-functinons.R.

There are two applications for these functions. On the one hand there is a set of functions, namely make-test-data-R and make-test-data-AR, that create a trainingsdataframe for the above functions to get an estimation for the real coverage probabilities. 

On the other hand there is stemcell data, that the functions are going to be applied to.

The main problem with the coverage probabilities is, that the number of iterations in the density simulations is way to low. This is due to running time problems. These problems in turn led to the creation of the function KB.minmax.poly.fast.
