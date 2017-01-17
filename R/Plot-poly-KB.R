# Diese Funktion erstellt die Plots für den Vergeich von Regressionsmodellen

Plot.poly.KB = function(data.set, degree, graphicspath){

  # Parameter, beta und sigma, bestimmen
  nobs = length(data.set)
  time=0:(nobs-1)/(nobs-1)
  time.2=time^2
  time.3=time^3
  time.4=time^4
  time.5=time^5
  time.6=time^6

  Y.gls.4 <- gls(data.set ~ time+I(time^2)+I(time^3)+I(time^4), correlation=corAR1())
  beta.4=Y.gls.4$coefficients
  sigma.4=Y.gls.4$sigma

  Y.gls.5 <- gls(data.set ~ time+I(time^2)+I(time^3)+I(time^4)+I(time^5), correlation=corAR1())
  beta.5=Y.gls.5$coefficients
  sigma.5=Y.gls.5$sigma

  Y.gls.6 <- gls(data.set ~ time+I(time^2)+I(time^3)+I(time^4)+I(time^5)+I(time^6), correlation=corAR1())
  beta.6=Y.gls.6$coefficients
  sigma.6=Y.gls.6$sigma

  # Modelle, inv.X.4, inv.X.5, inv.X.6, bestimmen
  X=matrix(data=NA,nrow=nobs,ncol=(degree[1]+1))
  for(j in 1:nobs){
    for(i in 1:(degree[1]+1)){X[j,i]=time[j]^(i-1)}
  }
  X.mat=t(X) %*% X
  inv.X.4=solve(X.mat)

  X=matrix(data=NA,nrow=nobs,ncol=(degree[2]+1))
  for(j in 1:nobs){
    for(i in 1:(degree[2]+1)){X[j,i]=time[j]^(i-1)}
  }
  X.mat=t(X) %*% X
  inv.X.5=solve(X.mat)

  X=matrix(data=NA,nrow=nobs,ncol=(degree[3]+1))
  for(j in 1:nobs){
    for(i in 1:(degree[3]+1)){X[j,i]=time[j]^(i-1)}
  }
  X.mat=t(X) %*% X
  inv.X.6=solve(X.mat)

  # allgemeine Werte
  alpha=0.05
  niter=250

  # Designmatritzen anpassen
  Delta.mat.4.5=Delta(inv.X.4, inv.X.5)
  Delta.mat.4.6=Delta(inv.X.4, inv.X.6)
  Delta.mat.5.6=Delta(inv.X.5, inv.X.6)

  # Koeffizientenvektor anpassen
  # beta.4.5.4 enth?lt den 4.5 angepasste 4er Beta-Vektor
  beta.4.5=Beta.calc(beta.4, beta.5)
  beta.4.5.4=beta.4.5[[1]]
  beta.4.5.5=beta.4.5[[2]]
  beta.4.6=Beta.calc(beta.4, beta.6)
  beta.4.6.4=beta.4.6[[1]]
  beta.4.6.6=beta.4.6[[2]]
  beta.5.6=Beta.calc(beta.5, beta.6)
  beta.5.6.5=beta.5.6[[1]]
  beta.5.6.6=beta.5.6[[2]]

  # kritischen Werte bestimmen
  # alpha, nobs, grad, niter, inv.X, a, b, ngridpoly
  par.bsp.vergl.4.5=KB.poly.fast(alpha, length(data.set), degree[2], niter, Delta.mat.4.5[[1]], a=0, b=1,
                                 ngridpoly = length(data.set))
  par.bsp.vergl.4.6=KB.poly.fast(alpha, length(data.set), degree[3], niter, Delta.mat.4.6[[1]], a=0, b=1,
                                 ngridpoly = length(data.set))
  par.bsp.vergl.5.6=KB.poly.fast(alpha, length(data.set), degree[3], niter, Delta.mat.5.6[[1]], a=0, b=1,
                                 ngridpoly = length(data.set))


  # Konfidenzb?nder berechnen
  #data.1, data.2, grad, delta.mat, beta.1, beta.2, sigma.1, sigma.2, factor, ngrid
  plot.bsp.vergl.4.5=plot.KB.vergl(data.set, data.set, degree[2], Delta.mat.4.5[[1]], beta.4.5.4, beta.4.5.5,
                                   sigma.4, sigma.5, par.bsp.vergl.4.5[[1]], ngrid = length(data.set))
  plot.bsp.vergl.4.6=plot.KB.vergl(data.set, data.set, degree[3], Delta.mat.4.6[[1]], beta.4.6.4, beta.4.6.6,
                                   sigma.4, sigma.6, par.bsp.vergl.4.6[[1]], ngrid = length(data.set))
  plot.bsp.vergl.5.6=plot.KB.vergl(data.set, data.set, degree[3], Delta.mat.5.6[[1]], beta.5.6.5, beta.5.6.6,
                                   sigma.5, sigma.6, par.bsp.vergl.5.6[[1]], ngrid = length(data.set))



  ################################
  # Graphiken erzeugen
  gp=paste(graphicspath,"-4-5.pdf",sep="")
  pdf(file=gp, width = 10, height = 8)
  # Grad 4 vs Grad 5
  plot(0,0,xlim=c(0,1),ylim=c(min(plot.bsp.vergl.4.5[[2]])-0.01,max(plot.bsp.vergl.4.5[[3]])+0.01),
       xlab="relative Zeit", ylab="relativer Unterschied", cex=2, lwd=3, cex.axis=2, cex.lab=2)
  lines(c(1,0),c(0,0), lwd=3)
  curve(beta.4.5.4[1]-beta.4.5.5[1] + (beta.4.5.4[2]-beta.4.5.5[2])*x+
          (beta.4.5.4[3]-beta.4.5.5[3])*x^2+(beta.4.5.4[4]-beta.4.5.5[4])*x^3+
          (beta.4.5.4[5]-beta.4.5.5[5])*x^4+(beta.4.5.4[6]-beta.4.5.5[6])*x^5,col="black", add=T, lwd=3)
  lines(plot.bsp.vergl.4.5[[1]], plot.bsp.vergl.4.5[[2]], col="black", lwd=3)
  lines(plot.bsp.vergl.4.5[[1]], plot.bsp.vergl.4.5[[3]], col="black", lwd=3)
  dev.off()

  gp=paste(graphicspath,"-4-6.pdf",sep="")
  pdf(file=gp, width = 10, height = 8)
  # Grad 4 vs Grad 6
  plot(0,0,xlim=c(0,1),ylim=c(min(plot.bsp.vergl.4.6[[2]])-0.01,max(plot.bsp.vergl.4.6[[3]])+0.01),
       xlab="relative Zeit", ylab="relativer Unterschied", cex=2, lwd=3, cex.axis=2, cex.lab=2)
  lines(c(1,0),c(0,0), lwd=3)
  curve(beta.4.6.4[1]-beta.4.6.6[1] + (beta.4.6.4[2]-beta.4.6.6[2])*x+
          (beta.4.6.4[3]-beta.4.6.6[3])*x^2+(beta.4.6.4[4]-beta.4.6.6[4])*x^3+
          (beta.4.6.4[5]-beta.4.6.6[5])*x^4+(beta.4.6.4[6]-beta.4.6.6[6])*x^5+
          (beta.4.6.4[7]-beta.4.6.6[7])*x^6,col="black", add=T, lwd=3)
  lines(plot.bsp.vergl.4.6[[1]], plot.bsp.vergl.4.6[[2]], col="black", lwd=3)
  lines(plot.bsp.vergl.4.6[[1]], plot.bsp.vergl.4.6[[3]], col="black", lwd=3)
  dev.off()

  gp=paste(graphicspath,"-5-6.pdf",sep="")
  pdf(file=gp, width = 10, height = 8)
  # Grad 5 vs Grad 6
  plot(0,0,xlim=c(0,1),ylim=c(min(plot.bsp.vergl.5.6[[2]])-0.01,max(plot.bsp.vergl.5.6[[3]])+0.01),
       xlab="relative Zeit", ylab="relativer Unterschied", cex=2, lwd=3, cex.axis=2, cex.lab=2)
  lines(c(1,0),c(0,0), lwd=3)
  curve(beta.5.6.5[1]-beta.5.6.6[1] + (beta.5.6.5[2]-beta.5.6.6[2])*x+
          (beta.5.6.5[3]-beta.5.6.6[3])*x^2+(beta.5.6.5[4]-beta.5.6.6[4])*x^3+
          (beta.5.6.5[5]-beta.5.6.6[5])*x^4+(beta.5.6.5[6]-beta.5.6.6[6])*x^5+
          (beta.5.6.5[7]-beta.5.6.6[7])*x^6,col="black", add=T, lwd=3)
  lines(plot.bsp.vergl.5.6[[1]], plot.bsp.vergl.5.6[[2]], col="black", lwd=3)
  lines(plot.bsp.vergl.5.6[[1]], plot.bsp.vergl.5.6[[3]], col="black", lwd=3)
  dev.off()

}
