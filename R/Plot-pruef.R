# Erstellt die Plots für Stammzelldaten für Prüfprobleme (assesing part of a regression model)

Plot.pruef = function(data, grad, k, graphicspath){

  # load correct data.set
  if(data=="Y.10"){data.set=Y.10}else if(data=="Y.30"){data.set=Y.30}

  alpha=0.05
  niter=1000
  nobs=length(data.set)
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


  Y.gls.5 <- gls(data.set ~ time+time.2+time.3+time.4+time.5, correlation=corAR1())
  aux.1 = exp(summary(Y.gls.5)$modelStruct[[1]][1])
  phi.5 = (aux.1 - 1) / (aux.1 + 1)

  # X.inv.trafo in Abhängigkeit von phi bestimmen
  R=Upsilon_fun(phi.5, nobs)[[1]]
  inv.trafo.R=sqrt_inv_mat(R)[[1]]
  X.trafo= inv.trafo.R %*% X
  X.mat.trafo=t(X.trafo) %*% X.trafo
  X.mat.inv.5=solve(X.mat.trafo)


  Y.gls.6 <- gls(data.set ~ time+time.2+time.3+time.4+time.5+time.6, correlation=corAR1())
  aux.1 = exp(summary(Y.gls.6)$modelStruct[[1]][1])
  phi.6 = (aux.1 - 1) / (aux.1 + 1)

  # X.inv.trafo in Abhängigkeit von phi bestimmen
  R=Upsilon_fun(phi.6, nobs)[[1]]
  inv.trafo.R=sqrt_inv_mat(R)[[1]]
  X.trafo= inv.trafo.R %*% X
  X.mat.trafo=t(X.trafo) %*% X.trafo
  X.mat.inv.6=solve(X.mat.trafo)

  if(grad==5){beta=Y.gls.5$coeff
  }else if(grad==6){beta=Y.gls.6$coeff
  }else{return("error")}

  if(grad==5){sigma=Y.gls.5$sigma
  }else if(grad==6){sigma=Y.gls.6$sigma
  }else{return("error")}

  if(grad==5){X.mat.inv=X.mat.inv.5
  }else if(grad==6){X.mat.inv=X.mat.inv.6
  }else{return("error")}

  I.tilde=I_tilde(k, grad)[[1]]
  V=I.tilde %*% X.mat.inv %*% t(I.tilde)

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
  if (plot.KB.R[[2]][nobs] > 0 && plot.KB.R[[3]][nobs] > 0)
  {legendplace="topleft"}else
      if (plot.KB.R[[2]][nobs] < 0 && plot.KB.R[[3]][nobs] < 0)
      {legendplace="bottomleft"}else if(data == "Y.30" && k == 2)
          {legendplace="topleft"}else if(data == "Y.30" && grad == 5)
          {legendplace="topleft"}else
          {legendplace="bottomleft"}

  pdf(graphicspath, width=10,height=8)
  par(mar=c(5.1,5.1,4.1,2.1))
  plot(x, x.2 %*% beta.2, type="l", xlab="relative Zeit", ylab="relatives Wachstum",
       cex=2, lwd=3, cex.axis=2, cex.lab=2, lty="dashed", ylim=c(min(plot.KB.R[[2]]),max(plot.KB.R[[3]])))
  lines(c(1,0),c(0,0), lwd=3, lty="dotted")
  lines(x, plot.KB.R[[2]], lty="solid", lwd=3)
  lines(x, plot.KB.R[[3]], lty="solid", lwd=3)
  legend(x=legendplace, legend=c("x.2 %*% beta.2", "Konfidenzband", "Nullfunktion"),
         col=c("black", "black", "black"),cex=2, lwd=3, lty=c("solid", "dashed", "dotted"))
  dev.off()
}
