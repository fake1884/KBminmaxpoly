##########################################
# erster Satz an Coverage.prob.fun functions
Coverage.prob.fun("R", "R")

Coverage.prob.fun("R", "minmax", niter = 100)

cov=NA
system.time({cov=Coverage.prob.fun("R", "minmax-poly", niter = 100)})

cov=NA
system.time({cov=Coverage.prob.fun("R", "minmax-poly-fast", niter = 100, ngridpoly = 100)})


##########################################
# zweiter Satz an Coverage.prob.fun functions

Coverage.prob.fun("AR-bekannt", "R")

Coverage.prob.fun("AR-bekannt", "minmax", niter = 100)

Coverage.prob.fun("AR-bekannt", "minmax-poly", niter = 100)

Coverage.prob.fun("AR-bekannt", "minmax-poly-fast", niter = 100, ngridpoly = 100)


##########################################
# dritter Satz an Coverage.prob.fun functions

Coverage.prob.fun("AR", "R")

Coverage.prob.fun("AR", "minmax", niter = 100)

Coverage.prob.fun("AR", "minmax-poly", niter = 100)

Coverage.prob.fun("AR", "minmax-poly-fast", niter = 100, ngridpoly = 100)



