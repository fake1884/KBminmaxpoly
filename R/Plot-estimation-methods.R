# plots all the three confidence bands into one graphic for given data

Plot.estimation.methods = function(data.set, degree, graphicspath){

  # do the estimation
  nobs = length(data.set)
  time=0:(nobs-1)/(nobs-1)
  time.2=time^2
  time.3=time^3
  time.4=time^4
  time.5=time^5

  #time.erg = rep(0, nobs)
  #for(i in 1:degree){time.erg = I(time) + I(time^i)}

  Y.gls <- gls(data.set ~ time+time.2+time.3+time.4+time.5, correlation=corAR1())
  beta=Y.gls$coefficients
  sigma=Y.gls$sigma
  aux.1 = exp(summary(Y.gls)$modelStruct[[1]][1])
  phi = (aux.1 - 1) / (aux.1 + 1)


  # do the model generation
  X=matrix(data=NA,nrow=nobs,ncol=(degree+1))
  for(j in 1:nobs){
    for(i in 1:(degree+1)){X[j,i]=time[j]^(i-1)}
  }

  R=Upsilon_fun(phi, nobs)[[1]]
  inv.trafo.R=sqrt_inv_mat(R)[[1]]
  X.trafo= inv.trafo.R %*% X
  X.mat.trafo=t(X.trafo) %*% X.trafo
  inv.X=solve(X.mat.trafo)

  # initialize fixed values
  alpha=0.05
  niter=1000
  ngridpoly=500

  # kritische Werte bestimmen
  # alpha, data, grad, inv.X
  par.KB.R <- KB.R(alpha, nobs, degree, inv.X)
  # alpha, y, grad, niter, inv.X, a, b
  par.KB.minmax <- KB.minmax(alpha, nobs, degree, niter, inv.X, a=0, b=1)
  # alpha, nobs, grad, niter, inv.X, a, b, ngridpoly
  par.KB.poly <- KB.poly.fast(alpha, nobs, degree, niter, inv.X, a=0, b=1,
                              ngridpoly)

  # Konfidenzb?nder berechnen
  # nobs, grad, inv.X, beta, sigma, factor, ngrid
  plot.KB.R=plot.KB(length(data.set), degree, inv.X, beta, sigma, par.KB.R[[1]], ngrid = length(data.set))
  plot.KB.minmax=plot.KB(length(data.set), degree, inv.X, beta, sigma, par.KB.minmax[[1]],
                         ngrid = length(data.set))
  plot.KB.poly=plot.KB(length(data.set), degree, inv.X, beta, sigma, par.KB.poly[[1]],
                            ngrid = length(data.set))


  #########################################
  # plot der Daten
  pdf(graphicspath, width = 10, height = 8)
  time=1:144/144
  par(mar=c(5.1,5.1,4.1,2.1))
  plot(0,0,xlim=c(0,1),ylim=c(min(plot.KB.R[[2]]),max(plot.KB.R[[3]])), xlab="relative Zeit",
     ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
  lines(time,data.set, lwd=3)
  curve(beta[1]+beta[2]*x+beta[3]*x^2+beta[4]*x^3+beta[5]*x^4+beta[6]*x^5, add=T, lwd=3)
  lines(plot.KB.R[[1]], plot.KB.R[[2]], lty="solid", lwd=3)
  lines(plot.KB.R[[1]], plot.KB.R[[3]], lty="solid", lwd=3)
  lines(plot.KB.minmax[[1]], plot.KB.minmax[[2]], lty="dotted", lwd=3)
  lines(plot.KB.minmax[[1]], plot.KB.minmax[[3]], lty="dotted", lwd=3)
  lines(plot.KB.minmax[[1]], plot.KB.poly[[2]], lty="dashed", lwd=3)
  lines(plot.KB.minmax[[1]], plot.KB.poly[[3]], lty="dashed", lwd=3)
  legend(x="topleft", legend=c("KB R", "KB minmax", "KB minmax poly"),
         col=c("black", "black", "black"),cex=2, lwd=3, lty=c("solid", "dotted", "dashed"))
  dev.off()
}
