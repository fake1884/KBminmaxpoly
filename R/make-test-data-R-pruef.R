#

Make.data.R.pruef = function(){

  # Reproduzierbarkeit gewährleisten
  set.seed(100)

  # feste Werte initialisieren
  ntest=100
  grad=3
  nobs=200
  beta=c(10,5,-4,7)#,3,-4

  x.raw=c(0:(nobs-1))
  x=x.raw/max(x.raw)
  X=matrix(data=NA,nrow=nobs,ncol=(grad+1))
  for(j in 1:nobs){
    for(i in 1:(grad+1)){X[j,i]=x[j]^(i-1)}
  }

  #mu = X %*% beta
  sigma.true = 0.5#sqrt(var(mu))/2  ##choose SNR 1 (low) or 3 (medium)
  I = matrix(diag(nobs),nrow=nobs,ncol=nobs)

  # Werte des Konfidenzbandes initialisieren
  alpha=0.05
  ngrid=nobs
  X.mat=t(X) %*% X
  X.mat.inv=solve(X.mat)
  a=0
  b=1

  # Wir kennen den wahren Wert schon
  k=2
  I.tilde=I_tilde(k, grad)[[1]]
  V=I.tilde %*% X.mat.inv %*% t(I.tilde)
  beta.2=I.tilde %*% beta

  X.2 = X[,(grad+2-k):(grad+1)]

  data_R_pruef_true = X.2 %*% beta.2

  #########################################################################
  # Zufallsdaten erzeugen
  # matrix für das Testset
  data_R_pruef_test=matrix(rep(rep(NA, ntest), nobs), nrow=nobs)
  for(i in 1:ntest)
  {
    # wahre Werte erzeugen
    e=mvtnorm::rmvnorm(1,mean=rep(0,length(x.raw)),sigma.true[1]*I)

    data_R_pruef_test[,i]=X %*% beta + t(e)
  }

  #########################################################################
  # schätzer für das Testset
  data_modelR_pruef_estR_beta=matrix(rep(rep(NA, ntest), grad+1), nrow=grad+1)
  data_modelR_pruef_estR_sigma=matrix(rep(rep(NA, ntest), 1), nrow=1)
  for(i in 1:ntest)
  {
    # Parameter schätzen
    fit.1=lm(data_R_pruef_test[,i]~poly(x,degree=grad,raw=T))
    data_modelR_pruef_estR_beta[,i]=fit.1$coeff
    data_modelR_pruef_estR_sigma[i]=summary(fit.1)$sigma
  }

  ###############################################################################
  # Daten speichern
  support_data_R_pruef <- list(ntest, nobs, grad ,beta, x, X, sigma.true, alpha, ngrid, X.mat, X.mat.inv,
                         a, b, k, V)
  names(support_data_R_pruef) <- paste(c("ntest", "nobs", "grad" ,"beta", "x", "X", "sigma.true",
                                         "alpha", "ngrid", "X.mat", "X.mat.inv", "a", "b", "k", "V"), sep="")
  devtools::use_data(data_R_pruef_test, overwrite = T)
  devtools::use_data(data_R_pruef_true, overwrite = T)
  devtools::use_data(data_modelR_pruef_estR_beta, overwrite = T)
  devtools::use_data(data_modelR_pruef_estR_sigma, overwrite = T)
  devtools::use_data(support_data_R_pruef, overwrite = T)

}
