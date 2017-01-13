# KBminmaxpoly
A package for calculating confidence bands on a rectangular region. KB always stands for confidence bands and the rectangular region is usually from the minimum to the maximum of the regression model.

This is the uploaded version of my Bachelorthesis. This R package calculates confidence bands. This is done in a multistep process. Firstly the data is read and save it in /data as an .rda file. Furthermore the model, notabley the design matrix, is created and fixed values, like alpha, are initialised. Then the following steps are performed

1. Estimate the coefficients beta, sigma and if needed phi
2. Based on the model and the confidence level alpha calculate the critical constant c
3. Use the critical constant c to calculate the confidence bands

Then there is a fitht step to create a plot or to calculate a coverage probability.

While the first and the last step are different for each calculation and therefore have different R scripts, details will be below each numbered step above has a corresponding R script in /R.

1. Thoug usually the R intern functions lm and gls are used, I wrote this function by hand to see how they work. There also is a fgls function. All this functions can be found in /R/estimation-functions.R.
2. This functios can be found in /R/crit-par-functions.R There are the following subcases
* KB.R which calculates the critical constant of a confidence band on the whole of R
* KB.R.pruef
* KB.minmax
* KB.minmax.poly
* KB.minmax.poly.fast
3. Furthermore, there are three functions to calculate the confidence bands once the critical constant is calculated. This functios can be found in /R/plot-functions.R
* plot.KB is the generic function to calculate confidence bands 
* plot.KB.vergl is a special function if two regerssion models are to be compared (vergl for german vergleichen)
* plot.KB.pruef is a special function if part of a regression model has to be assesed (pruef for german prüfen)

The fitht step of giving out the result 

There are a bunch of supplementary functions like estimation functions in estimation-functions.R. In addition, there are support functions in support-functinons.R.


Now let us discuss where this functios are used and where the respective functios can be found. There are two applications for these functions. On the one hand there is a set of functions, namely make-test-data-R and make-test-data-AR, that create a trainingsdataframe for the above functions to get an estimation for the real coverage probabilities. 

On the other hand there is stemcell data, that the functions are going to be applied to.


The main problem with the coverage probabilities is, that the number of iterations in the density simulations is way to low. This is due to running time problems. These problems in turn led to the creation of the function KB.minmax.poly.fast.
