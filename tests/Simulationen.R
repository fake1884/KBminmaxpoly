##########################################
# erster Satz an Coverage.prob.fun functions
# last run cov.R.R= 953, system.time= 13.479
cov.R.R=NA
system.time({cov.R.R=Coverage.prob.fun("R", "R")})

# last run niter = 1000 cov.R.minmax= 953, system.time = 14.507
cov.R.minmax=NA
system.time({cov.R.minmax=Coverage.prob.fun("R", "minmax", niter = 1000)})

# diese Funktion scheint mittlerweile nicht mehr zu funktionieren
# last run niter = 1000 poly.R= , system.time =
cov.R.poly=NA
system.time({cov.R.poly=Coverage.prob.fun("R", "minmax-poly", niter = 1000)})

# last run niter = 1000, ngridpoly = 500, cov.poly.fast.R = 951, system.time = 21.920
cov.R.poly.fast=NA
system.time({cov.R.poly.fast=Coverage.prob.fun("R", "minmax-poly-fast", niter = 1000, ngridpoly = 500)})


##########################################
# zweiter Satz an Coverage.prob.fun functions

# last run cov.R.AR.bekannt = , system.time=
cov.R.AR.bekannt=NA
system.time({cov.R.AR.bekannt = Coverage.prob.fun("AR-bekannt", "R") })

# last run niter = 100 cov.R.minmax= 1000, system.time=
cov.minmax.AR.bekannt=NA
system.time({cov.minmax.AR.bekannt = Coverage.prob.fun("AR-bekannt", "minmax", niter = 100)})

# diese Funktion scheint mittlerweile nicht mehr zu funktionieren
# last run niter = 100 cov.R.minmax= 1000, system.time=
cov.poly.AR.bekannt=NA
system.time({cov.poly.AR.bekannt = Coverage.prob.fun("AR-bekannt", "minmax-poly", niter = 100)})

# last run niter=1000, ngridpoly=500, cov.poly.fast.AR.bekannt = 1000, system.time = 10642.367
cov.poly.fast.AR.bekannt=NA
system.time({cov.poly.fast.AR.bekannt=Coverage.prob.fun("AR-bekannt", "minmax-poly-fast",
                                                        niter = 1000, ngridpoly = 500)})


##########################################
# dritter Satz an Coverage.prob.fun functions

# last run cov.R.R = 97.8, system.time = 15.634
cov.R.AR=NA
system.time({cov.R.AR = Coverage.prob.fun("AR", "R")})

# last run niter = 1000, ngridpoly = 500, cov.minmax.AR = 96.6, system.time = 81.613
cov.minmax.AR=NA
system.time({cov.minmax.AR = Coverage.prob.fun("AR", "minmax", niter = 100)})

# diese Funktion scheint mittlerweile nicht mehr zu funktionieren
# last run niter=, ngridpoly=, cov.poly.fast.AR= , system.time=
cov.poly.AR=NA
system.time({cov.poly.AR=Coverage.prob.fun("AR", "minmax-poly", niter = 100)})

# last run niter = 1000, ngridpoly = 500, cov.poly.fast.AR= 93.9, system.time = 8274.412
cov.poly.fast.AR=NA
system.time({cov.poly.fast.AR=Coverage.prob.fun("AR", "minmax-poly-fast", niter = 1000, ngridpoly = 1000)})




