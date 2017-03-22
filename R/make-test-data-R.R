# diese Funktion wurde benutzt, um die im Paket enthaltenen Testdaten für R zu erzeugen
#

Make.data.R = function()
{
  # während der for-Schleife um die neuen Daten für R als Modell zu simulieren

    # Reproduzierbarkeit gewährleisten
    set.seed(100)

    # feste Werte initialisieren
    # besser 1000
    #ntest=100
    ntest=1000
    grad=1
    #grad=5
    # höherer Stichprobenumfang -> 200
    #nobs=50
    nobs=200
    beta=c(10,5)
    #beta=c(10,5,-4,7,3,-4)

    x.raw=c(0:(nobs-1))
    x=x.raw/max(x.raw)
    X=matrix(data=NA,nrow=nobs,ncol=(grad+1))
    for(j in 1:nobs){
      for(i in 1:(grad+1)){X[j,i]=x[j]^(i-1)}
    }

    mu=X%*%beta
    sigma.true=0.5
    #sigma.true=sqrt(var(mu))/2  ##choose SNR 1 (low) or 3 (medium)
    I=matrix(diag(nobs),nrow=nobs,ncol=nobs)

    # Wir kennen den wahren Wert schon
    data_R_true <- (X %*% beta)

    # Werte des Konfidenzbandes initialisieren
    alpha=0.05
    ngrid=nobs
    X.mat=t(X) %*% X
    X.mat.inv=solve(X.mat)
    a=0
    b=1

    #########################################################################
    # Zufallsdaten erzeugen
    # matrix für das Testset
    data_R_test=matrix(rep(rep(NA, ntest), nobs), nrow=nobs)
    for(i in 1:ntest)
    {
      # wahre Werte erzeugen
      e=mvtnorm::rmvnorm(1,mean=rep(0,length(x.raw)),sigma.true[1]*I)

      data_R_test[,i]=X %*% beta + t(e)
    }

    #########################################################################
    # schätzer für das Testset
    data_modelR_estR_beta=matrix(rep(rep(NA, ntest), grad+1), nrow=grad+1)
    data_modelR_estR_sigma=matrix(rep(rep(NA, ntest), 1), nrow=1)
    for(i in 1:ntest)
    {
      # Parameter schätzen
      fit.1=lm(data_R_test[,i]~poly(x,degree=grad,raw=T))
      data_modelR_estR_beta[,i]=fit.1$coeff
      data_modelR_estR_sigma[i]=summary(fit.1)$sigma
    }

    ###############################################################################
    # Daten speichern
    support_data_R <- list(ntest, nobs, grad ,beta, x, X, sigma.true, alpha, ngrid, X.mat, X.mat.inv,
                           a, b)
    names(support_data_R) <- paste(c("ntest", "nobs", "grad" ,"beta", "x", "X", "sigma.true", "alpha", "ngrid",
                            "X.mat", "X.mat.inv", "a", "b"), sep="")
    devtools::use_data(data_R_test, overwrite = T)
    devtools::use_data(data_R_true, overwrite = T)
    devtools::use_data(data_modelR_estR_beta, overwrite = T)
    devtools::use_data(data_modelR_estR_sigma, overwrite = T)
    devtools::use_data(support_data_R, overwrite = T)
}
