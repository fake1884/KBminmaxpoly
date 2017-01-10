##########################################
# erster Satz an Coverage.prob.fun functions
Coverage.prob.fun("R", "R")

Coverage.prob.fun("R", "minmax", niter = 50)

Coverage.prob.fun("R", "minmax-poly", niter = 10)

Coverage.prob.fun("R", "minmax-poly-fast", niter = 50, ngridpoly = 50)


##########################################
# zweiter Satz an Coverage.prob.fun functions

Coverage.prob.fun("AR-bekannt", "R")

Coverage.prob.fun("AR-bekannt", "minmax", niter = 50)

Coverage.prob.fun("AR-bekannt", "minmax-poly", niter = 10)

Coverage.prob.fun("AR-bekannt", "minmax-poly-fast", niter = 50, ngridpoly = 50)


##########################################
# dritter Satz an Coverage.prob.fun functions

Coverage.prob.fun("AR", "R")

Coverage.prob.fun("AR", "minmax", niter = 50)

Coverage.prob.fun("AR", "minmax-poly", niter = 10)

Coverage.prob.fun("AR", "minmax-poly-fast", niter = 50, ngridpoly = 50)



