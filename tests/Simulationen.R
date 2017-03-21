##########################################
# erster Satz an Coverage.prob.fun functions
Coverage.prob.fun("R", "R")

Coverage.prob.fun("R", "minmax", niter = 100)

cov.poly.R=NA
system.time({cov.poly.R=Coverage.prob.fun("R", "minmax-poly", niter = 100)})

# last run niter=5000, ngridpoly=100, col.poly.fast.AR= 93, system.time= 1104.185
# 95
cov.poly.fast.R=NA
system.time({cov.poly.fast.R=Coverage.prob.fun("R", "minmax-poly-fast", niter = 5000, ngridpoly = 100)})


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




