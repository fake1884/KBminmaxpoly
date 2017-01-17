# Erstellt die Plots für Stammzelldaten für Prüfprobleme (assesing part of a regression model)

Plot.pruef = function(data.set, grad, k, graphicspath){

  alpha=0.05
  niter=100
  nobs=length(Y.10)
  ngridpoly=nobs
  ngrid=nobs
  a=0 # diese Werte definieren A
  b=1
  time=0:(nobs-1)/(nobs-1)
  time.2=time^2
  time.3=time^3
  time.4=time^4
  time.5=time^5
  time.6=time^6
  alpha=0.05
  x.raw=c(0:(nobs-1))
  x=x.raw/max(x.raw)

  X=matrix(data=NA,nrow=nobs,ncol=(grad+1))
  for(j in 1:nobs){
    for(i in 1:(grad+1)){X[j,i]=x[j]^(i-1)}
  }
  X.mat=t(X) %*% X
  X.mat.inv=solve(X.mat)

  I.tilde=I_tilde(k, grad)[[1]]
  V=I.tilde %*% X.mat.inv %*% t(I.tilde)

  Y.gls.5 <- gls(data.set ~ time+time.2+time.3+time.4+time.5, correlation=corAR1())
  Y.gls.6 <- gls(data.set ~ time+time.2+time.3+time.4+time.5+time.6, correlation=corAR1())

  if(grad==5){beta=Y.gls.5$coeff
  }else if(grad==6){beta=Y.gls.6$coeff
  }else{return("error")}

  if(grad==5){sigma=Y.gls.5$sigma
  }else if(grad==6){sigma=Y.gls.6$sigma
  }else{return("error")}

  beta.2=I.tilde %*% beta

  if(k==1){x.2=x^(grad)
  }else if(k==2){x.2= matrix(c(x^(grad-1),x^(grad)), ncol=2)
  }else{return("error")}

  # kritischen Wert berechnen
  # alpha, nobs, grad, niter, inv.X, a, b, ngridpoly
  par.bsp.R=KB.poly.fast(alpha = 0.05, nobs, k-1, niter, V, a, b, ngridpoly)

  # Konfidenzband bestimmen
  plot.KB.R=plot.KB.pruef( nobs , grad, X.mat.inv , beta, sigma, factor = par.bsp.R[[1]], k, ngrid )

  # Graphik erzeugen
  pdf(graphicspath, width=10,height=8)
  plot(x, x.2 %*% beta.2, type="l", main="10 kPa", xlab="relative Zeit", ylab="relatives Wachstum",
       cex=2, lwd=3, cex.axis=2, cex.lab=2)
  lines(x, plot.KB.R[[2]], lty="solid", lwd=3)
  lines(x, plot.KB.R[[3]], lty="solid", lwd=3)
  dev.off()
}
