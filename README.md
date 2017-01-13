# KBminmaxpoly
A package for calculating confidence bands on a rectangular region. KB always stands for confidence bands and the rectangular region is usually from the minimum to the maximum of the regression model.

This is the uploaded version of my Bachelorthesis. This R package calculates confidence bands with the application of stem cell data, wich is included in /data in mind. There is also a test routine to calculate the coverage probability of the confidence bands.

This is done in a multistep process. Firstly the data is read and save it in /data as an .rda file. Furthermore the model, notabley the design matrix, is created and fixed values, like alpha, are initialised.  All of this is done in the files /R/namely make-test-data-R and /R/make-test-data-AR for the coverage probability and in /R/convert-stem data. Though for the stem data there is no central point, where the fixed values are initialized.

Then the following steps are performed

1. Estimate the coefficients beta, sigma and if needed phi
2. Based on the model and the confidence level alpha calculate the critical constant c
3. Use the critical constant c to calculate the confidence bands

Then there is a fitht step to create a plot or to calculate a coverage probability.

While the first and the last step are different for each calculation and therefore have different R scripts, details will be below each numbered step above has a corresponding R script in /R.

1. Thoug usually the R intern functions lm and gls are used, I wrote this function by hand to see how they work. There also is a fgls function. All this functions can be found in /R/estimation-functions.R.
2. This functios can be found in /R/crit-par-functions.R There are the following subcases
* KB.R which calculates the critical constant of a confidence band on the whole of R
* KB.R.pruef which does the same as above in the case of assesing part of a regression model
* KB.minmax calculates the critical constant on a rectangular region
* KB.minmax.poly also calculates the critical constant on a rectangular region, but additionally it is assumed, that there is a polynomial model
* KB.minmax.poly.fast does the same as above but instead of newton to calculate maxima gridsearch is used. This seemed to increase speed by a factor of five to ten, thoug there was no system.time trial done.
3. Furthermore, there are three functions to calculate the confidence bands once the critical constant is calculated. This functios can be found in /R/plot-functions.R
* plot.KB is the generic function to calculate confidence bands 
* plot.KB.vergl is a special function if two regerssion models are to be compared (vergl for german vergleichen)
* plot.KB.pruef is a special function if part of a regression model has to be assesed (pruef for german prüfen)

The fitht step of giving out the result is usually done in a two step process first there is a function in /R which is one of 

1. coverage-prob.R
2. coverage-prob-pruef.R
3. Plot-degrees.R plots polynomial regression models with different degree
4. Plot-estimation-methods.R plots the three different confidence bands that are generated abvove for a given problem in the same graphic
5. Plot-poly-KB creates the plots for the comparison of regeression models. In detail degree 4,5,6 are compared to each other resulting in the comparisons per data set
6. Plot-pruef creates the plots for the assesing problems. For degree five the beta.5 == 0 is assesed and for degree six beta.6 ==0 and beta.6 == beta.5 == 0 are assesed. This again results in three plots per data set.

The first two calculate the coverage probabilty for the different methods and models. The rest creates plots for the stem cell data. 

For the coverage probability functions there is an R script in /tests that calls the functions above with the right parameters. The same is true for the stem cell data with the difference, that the calling script is in /man.


There are a bunch of supplementary functions. Two important ones are support functions in R/support-functinons.R and a test evaluation function in /R/Test-function.R.


The main problem with the coverage probabilities is, that the number of iterations in the density simulations is way to low. This is due to running time problems. These problems in turn led to the creation of the function KB.minmax.poly.fast.
