# KBminmaxpoly
This is a package for calculating confidence bands on a rectangular region. KB always stands for confidence bands and the rectangular region is usually from the minimum to the maximum of the regression model.

This is the uploaded version of my bachelor thesis. This R package calculates confidence bands with the application of stem cell data, which is included in /data, in mind. There is also a test routine to calculate the coverage probability of the confidence bands.

This is done in a multistep process. Firstly, the data is read and saved in /data as an .rda file. Furthermore, the model, notably the design matrix, is created and fixed values, like alpha, are initialised.  All of this is done in the files /R/make-test-data-R.R, /R/make-test-data-R-pruef.R, /R/make-test-data-AR.R, /R/make-test-data-AR-pruef.R for the coverage probability and in /R/convert-stem-data.R for the stem data. However, for the stem data there is no central point where the fixed values are initialised.

Then the following steps are performed:

2. Estimate the coefficients beta, sigma and phi if needed.
3. Based on the model and the confidence level alpha, calculate the critical constant c.
4. Use the critical constant c to calculate the confidence bands.

Then there is a fifth step to create a plot or to calculate a coverage probability.

Details for each numbered step above are now discussed. Furthermore, notice that each step has a corresponding R script in /R.

2. Though usually the R intern functions lm and gls are used, I wrote those functions again to make sure they work correctly in the desired context. Also, there is a fgls function. All these functions can be found in /R/estimation-functions.R.
3. These functions can be found in /R/crit-par-functions.R There are the following subcases:
  * *KB.R* which calculates the critical constant of a confidence band on the whole of the real numbers.
  * *KB.R.pruef* which does the same as above in the case of assessing part of a regression model.
  * *KB.minmax* calculates the critical constant on a rectangular region.
  * *KB.minmax.poly* also calculates the critical constant on a rectangular region, but additionally it is assumed, that there is a polynomial model.
  * *KB.minmax.poly.fast* does the same as above but gridsearch is used instead of Newton to calculate maxima. This seemed to increase speed by a factor of five to ten, though there was no *system.time* trial done.
4. Furthermore, there are three functions to calculate the confidence bands, once the critical constant is calculated. This functions can be found in /R/plot-functions.R
  * *plot.KB* is the generic function to calculate confidence bands 
  * *plot.KB.vergl* is a special function to compare two regerssion models (vergl for german vergleichen)
  * *plot.KB.pruef* is a special function to assess part of a regression model (pruef for german prüfen)

The fifth step of giving out the result is usually done in a two step process. First, there is a function in /R which is one of 

1. *coverage-prob.R*
2. *coverage-prob-pruef.R*
3. *Plot-degrees.R* plots polynomial regression models with different degrees.
4. *Plot-estimation-methods.R* plots the three different confidence bands that are generated above for a given problem in the same graphic.
5. *Plot-poly-KB.R* creates the plots for the comparison of regeression models. In detail, degree four,five and six are compared to each other resulting in the comparisons per data set.
6. *Plot-pruef.R* creates the plots for the assessing problems. For degree five the beta.5 == 0 is assessed and for degree six beta.6 ==0 and beta.6 == beta.5 == 0 are assessed. This again results in three plots per data set.

The first two calculate the coverage probabilty for the different methods and models. The others create plots for the stem cell data. 

For the coverage probability functions there is an R script in /tests that calls the functions above with the right parameters. The same is true for the stem cell data with the difference, that the calling script is in /man.


There are a bunch of supplementary functions. Two important ones are support functions in R/support-functinons.R and a test evaluation function in /R/Test-function.R.


The main problem with the coverage probabilities is, that the number of iterations in the density simulations is way too low. This is due to running time problems. These problems in turn lead to the creation of the function KB.minmax.poly.fast, but the issue still persists.
