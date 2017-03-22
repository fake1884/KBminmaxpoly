##########################################
# erster Satz an Coverage.prob.fun functions
# last run col.R.R= 953, system.time= 13.479
cov.R.R=NA
system.time({cov.R.R=Coverage.prob.fun("R", "R")})

# last run niter = 1000 col.R.minmax= 956, system.time= 707.471
cov.R.minmax=NA
system.time({cov.R.minmax=Coverage.prob.fun("R", "minmax", niter = 1000)})

# diese Funktion scheint mittlerweile nicht mehr zu funktionieren
# last run niter = 1000 poly.R= , system.time=
cov.R.poly=NA
system.time({cov.R.poly=Coverage.prob.fun("R", "minmax-poly", niter = 1000)})

# last run niter=1000, ngridpoly=100, col.poly.fast.R= 951, system.time= 26303.91
cov.R.poly.fast=NA
system.time({cov.R.poly.fast=Coverage.prob.fun("R", "minmax-poly-fast", niter = 1000, ngridpoly = 100)})


##########################################
# zweiter Satz an Coverage.prob.fun functions

Coverage.prob.fun("AR-bekannt", "R")

Coverage.prob.fun("AR-bekannt", "minmax", niter = 100)

Coverage.prob.fun("AR-bekannt", "minmax-poly", niter = 100)

# last run niter=5000, ngridpoly=100, col.poly.fast.AR= 90, system.time=993.876
cov.poly.fast.AR.bekannt=NA
system.time({cov.poly.fast.AR.bekannt=Coverage.prob.fun("AR-bekannt", "minmax-poly-fast",
                                                        niter = 5000, ngridpoly = 100)})


##########################################
# dritter Satz an Coverage.prob.fun functions

Coverage.prob.fun("AR", "R")

Coverage.prob.fun("AR", "minmax", niter = 100)

cov.poly.AR=NA
system.time({cov.poly.AR=Coverage.prob.fun("AR", "minmax-poly", niter = 100)})

# last run niter=5000, ngridpoly=100, col.poly.fast.AR= 100, system.time=956.586
cov.poly.fast.AR=NA
system.time({cov.poly.fast.AR=Coverage.prob.fun("AR", "minmax-poly-fast", niter = 5000, ngridpoly = 100)})




