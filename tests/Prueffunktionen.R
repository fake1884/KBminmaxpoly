# Coverage.prob.pruef.fun <- function(model.type, est.method, niter, ngridpoly)

##########################################
# erster Satz an coverage.prob.pruef.fun functions

Coverage.prob.pruef.fun("R-pruef", "R")

Coverage.prob.pruef.fun("R-pruef", "minmax", niter = 100)

Coverage.prob.pruef.fun("R-pruef", "minmax-poly", niter = 100)

Coverage.prob.pruef.fun("R-pruef", "minmax-poly-fast", niter = 100, ngridpoly = 100)


##########################################
# zweiter Satz an coverage.prob.pruef.fun functions

Coverage.prob.pruef.fun("AR-bekannt-pruef", "R")

Coverage.prob.pruef.fun("AR-bekannt-pruef", "minmax", niter = 100)

Coverage.prob.pruef.fun("AR-bekannt-pruef", "minmax-poly", niter = 100)

Coverage.prob.pruef.fun("AR-bekannt-pruef", "minmax-poly-fast", niter = 100, ngridpoly = 100)


##########################################
# dritter Satz an coverage.prob.pruef.fun functions

Coverage.prob.pruef.fun("AR-pruef", "R")

Coverage.prob.pruef.fun("AR-pruef", "minmax", niter = 100)

Coverage.prob.pruef.fun("AR-pruef", "minmax-poly", niter = 100)

Coverage.prob.pruef.fun("AR-pruef", "minmax-poly-fast", niter = 100, ngridpoly = 100)



