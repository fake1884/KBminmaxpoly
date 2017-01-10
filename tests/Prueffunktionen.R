# Coverage.prob.pruef.fun <- function(model.type, est.method, niter, ngridpoly)

##########################################
# erster Satz an coverage.prob.pruef.fun functions

Coverage.prob.pruef.fun("R-pruef", "R")

Coverage.prob.pruef.fun("R-pruef", "minmax", 50)

Coverage.prob.pruef.fun("R-pruef", "minmax-poly", 10)

Coverage.prob.pruef.fun("R-pruef", "minmax-poly-fast", 50, 10)


##########################################
# zweiter Satz an coverage.prob.pruef.fun functions

Coverage.prob.pruef.fun("AR-bekannt-pruef", "R")

Coverage.prob.pruef.fun("AR-bekannt-pruef", "minmax", 50)

Coverage.prob.pruef.fun("AR-bekannt-pruef", "minmax-poly", 10)

Coverage.prob.pruef.fun("AR-bekannt-pruef", "minmax-poly-fast", 50, 10)


##########################################
# dritter Satz an coverage.prob.pruef.fun functions

Coverage.prob.pruef.fun("AR-pruef", "R")

Coverage.prob.pruef.fun("AR-pruef", "minmax", 50)

Coverage.prob.pruef.fun("AR-pruef", "minmax-poly", 10)

Coverage.prob.pruef.fun("AR-pruef", "minmax-poly-fast", 50, 10)



