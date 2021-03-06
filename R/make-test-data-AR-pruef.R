# diese Funktion wurde benutzt, um die im Paket enthaltenen Testdaten für AR/AR-bekannt pruef zu erzeugen

Make.data.AR.pruef = function(){

  # Anzahl an Testdurchl?ufen
  ntest=1000

  # Reproduzierbarkeit gew?hrleisten
  set.seed(100)

  # feste Werte initialisieren
  grad=3
  nobs=50
  beta.true=c(10,5,-4,7)#,3,-4
  sigma.true=0.007545373
  phi.true=0.8225374  # bestimmt die korrelation
  x.raw=c(0:(nobs-1))
  x=x.raw/max(x.raw)

  X=matrix(data=NA,nrow=nobs,ncol=(grad+1))
  for(j in 1:nobs){
    for(i in 1:(grad+1)){X[j,i]=x[j]^(i-1)}
  }
  Upsilon = Upsilon_fun(phi.true, nobs)[[1]]
  Upsilon= Upsilon * sigma.true


  # Werte des Konfidenzbandes initialisieren
  alpha=0.05
  niter=100
  ngrid=nobs
  a=0 # diese Werte definieren A
  b=1
  time=0:(nobs-1)/(nobs-1)
  time.2=time^2
  time.3=time^3
  time.4=time^4
  time.5=time^5
  alpha=0.05
  ngrid=nobs
  X.mat=t(X) %*% X
  X.mat.inv=solve(X.mat)

  # Wir kennen den wahren Wert des Regressionsmodells schon
  k=1
  I.tilde=I_tilde(k, grad)[[1]]
  V=I.tilde %*% X.mat.inv %*% t(I.tilde)
  beta.2=I.tilde %*% beta.true

  X.2 = X[,(grad+2-k):(grad+1)]

  data_R_pruef_true = X.2 %*% beta.2


  ######################################################
  # matrix für das Testset
  data_AR_pruef_test=matrix(rep(rep(NA, ntest), nobs), nrow=nobs)
  for(i in 1:ntest)
  {
    # wahre Werte erzeugen
    e=mvtnorm::rmvnorm(1,mean=rep(0,nobs),Upsilon)

    data_AR_pruef_test[,i]=X %*% beta.true + t(e)
  }

  #############################################################
  # schätzer für das Testset AR-bekannt
  data_modelAR_pruef_estAR_bekannt_beta=matrix(rep(rep(NA, ntest), grad+1), nrow=grad+1)
  data_modelAR_pruef_estAR_bekannt_sigma=matrix(rep(rep(NA, ntest), 1), nrow=1)
  for(i in 1:ntest)
  {
    # Parameter schätzen
    y = data_AR_pruef_test[,i]
    fit.1 = ar.1(grad, y, phi.true)
    data_modelAR_pruef_estAR_bekannt_beta[,i]=fit.1[[2]]
    data_modelAR_pruef_estAR_bekannt_sigma[i]=fit.1[[3]]
  }

  #############################################################
  # schätzer für das Testset AR
  data_modelAR_pruef_estAR_beta=matrix(rep(rep(NA, ntest), grad+1), nrow=grad+1)
  data_modelAR_pruef_estAR_sigma=matrix(rep(rep(NA, ntest), 1), nrow=1)
  data_modelAR_estAR_X_trafo_inv=rep(list(matrix(rep(rep(NA, nobs), nobs), nrow=nobs)), ntest)
  for(i in 1:ntest)
  {
    # Parameter schätzen
    y = data_AR_pruef_test[,i]
    fit.1 = gls(y ~ time+time.2+time.3, correlation=corAR1())
    data_modelAR_pruef_estAR_beta[,i]=fit.1$coeff
    data_modelAR_pruef_estAR_sigma[i]=summary(fit.1)$sigma
    # keine Ahnung, warum man das so berechnen muss
    aux.1 = exp(summary(fit.1)$modelStruct[[1]][1])
    phi = (aux.1 - 1) / (aux.1 + 1)

    # X.inv.trafo in Abhängigkeit von phi bestimmen
    R=Upsilon_fun(phi, nobs)[[1]]
    inv.trafo.R=sqrt_inv_mat(R)[[1]]
    X.trafo= inv.trafo.R %*% X
    X.mat.trafo=t(X.trafo) %*% X.trafo
    data_modelAR_estAR_X_trafo_inv[[i]]=solve(X.mat.trafo)
  }

  ################################################################
  # Daten speichern
  support_data_AR_pruef <- list(ntest, grad, nobs, beta.true, x, X, sigma.true, phi.true,
                          alpha, ngrid, X.mat, X.mat.inv, a, b, k, V)
  names(support_data_AR_pruef) <- paste(c("ntest", "grad", "nobs", "beta.true", "x", "X", "sigma.true", "phi.true",
                                    "alpha", "ngrid", "X.mat", "X.mat.inv", "a", "b", "k", "V"), sep = "")
  devtools::use_data(support_data_AR_pruef, overwrite = T)

  # Allgemeine Werte
  devtools::use_data(data_AR_pruef_test, overwrite = T)
  devtools::use_data(data_AR_pruef_true, overwrite = T)

  # AR bekannt
  devtools::use_data(data_modelAR_pruef_estAR_bekannt_beta, overwrite = T)
  devtools::use_data(data_modelAR_pruef_estAR_bekannt_sigma, overwrite = T)

  # AR
  devtools::use_data(data_modelAR_pruef_estAR_beta, overwrite = T)
  devtools::use_data(data_modelAR_pruef_estAR_sigma, overwrite = T)
  devtools::use_data(data_modelAR_estAR_X_trafo_inv, overwrite = T)
}
