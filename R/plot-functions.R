##############################
# Konfidenzband berechnen


# Diese Funktion erzeugt die obere und untere Grenze lower und upper eines Konfidenzbandes bei
# vorgegebenen Parametern.
# Ben?tigt werden die rohen Daten, da ich Datenpunkte im Abstand 1/length(data) bestimme, der Grad des
# Polynoms, um die x-Werte zu erzeugen und nat?rlich inv.X, beta und sigma.
# Die Berechnung erfolgt wie zum Beispiel in der Einleitung zu Kapitel 1.4 angegeben.
# R?ckgabewerte wie oben.

plot.KB <- function(nobs, grad, inv.X, beta, sigma, factor, ngrid){


  # feste Werte erzeugen
  step <- 1/(ngrid-1)
  points = seq(from=0,to=1,by=step)
  lower <- rep(NA, nobs)
  upper <- rep(NA, nobs)



  for(i in 1:ngrid)
  {
    x=matrix(data=NA, ncol=1, nrow=(grad+1))
    for(j in 1:(grad+1)){x[j,]=points[i]^(j-1)}
    lower[i] = (t(x) %*% beta - factor * sigma * sqrt(t(x) %*% inv.X %*% x))
    upper[i] = (t(x) %*% beta + factor * sigma * sqrt(t(x) %*% inv.X %*% x))
  }
  erg=list(points,lower,upper)
  erg
}




##############################
# Konfidenzband berechnen im Vergleich


# Diese Funktion erzeugt die obere und untere Grenze lower und upper eines Konfidenzbandes bei
# vorgegebenen Parametern f?r den Vergleich von Konfidenzb?ndern
# Ben?tigt werden die rohen Daten, da ich Datenpunkte im Abstand 1/length(data) bestimme, der Grad des
# Polynoms, um die x-Werte zu erzeugen und nat?rlich inv.X, beta und sigma.
# Die Berechnung erfolgt wie zum Beispiel in der Einleitung zu Kapitel 1.4 angegeben.
# R?ckgabewerte wie oben.

plot.KB.vergl <- function(data.1, data.2, grad, delta.mat, beta.1, beta.2, sigma.1, sigma.2, factor, ngrid){


  # sigmas nach Formel auf Mittelwert zusammenrechnen
  n.1=length(data.1)
  n.2=length(data.2)
  sigma=(n.1-grad-1)/(n.1+n.2-2*(grad+1))*sigma.1 + (n.2-grad-1)/(n.1+n.2-2*(grad+1))*sigma.2

  # betas auf die richtige L?nge bringen
  while(length(beta.1)<length(beta.2))
  {
    beta.1=as.matrix(c(beta.1,0))
  }
  while(length(beta.2)<length(beta.1))
  {
    beta.2=as.matrix(c(beta.2,0))
  }

  # feste Werte erzeugen
  nsteps=ngrid
  step <- 1/nsteps
  start <- 0
  temp <- start
  lower <- numeric()
  upper <- numeric()
  points <- numeric()
  while(temp<1)
  {
    x=numeric()
    for(i in 1:(grad+1)){x=matrix(c(x,temp^(i-1)),nrow=i)}
    lower <- c(lower,t(x) %*% beta.1 - t(x) %*% beta.2 - factor * sigma * sqrt(t(x) %*% delta.mat %*% x))
    upper <- c(upper,t(x) %*% beta.1 - t(x) %*% beta.2 + factor * sigma * sqrt(t(x) %*% delta.mat %*% x))
    points <- c(points,temp)
    temp <- temp + step
  }
  erg=list(points,lower,upper)
  erg
}





##############################
# Konfidenzband berechnen


# Diese Funktion erzeugt die obere und untere Grenze lower und upper eines Konfidenzbandes bei
# vorgegebenen Parametern.
# Ben?tigt werden die rohen Daten, da ich Datenpunkte im Abstand 1/length(data) bestimme, der Grad des
# Polynoms, um die x-Werte zu erzeugen und nat?rlich inv.X, beta und sigma.
# Die Berechnung erfolgt wie zum Beispiel in der Einleitung zu Kapitel 1.4 angegeben.
# R?ckgabewerte wie oben.

plot.KB.pruef <- function(nobs, grad, inv.X, beta, sigma, factor, k, ngrid){

  # die A Matrix im Buch ist I.tilde
  I.tilde=I_tilde(k, grad)[[1]]
  V=I.tilde %*% inv.X %*% t(I.tilde)
  beta.2=I.tilde %*% beta

  # feste Werte erzeugen
  step <- 1/(ngrid-1)
  lower <- rep(NA, nobs)
  upper <- rep(NA, nobs)
  points = seq(from=0,to=1,by=step)

  for(i in 1:ngrid)
  {
    x=matrix(data=NA, ncol=1, nrow=k)
    for(j in 1:k){x[j,]=points[i]^(grad+1-k)}

    lower[i] <- (t(x) %*% beta.2 - factor * sigma * sqrt(t(x) %*% V %*% x))
    upper[i] <- (t(x) %*% beta.2 + factor * sigma * sqrt(t(x) %*% V %*% x))

  }

  erg=list(points,lower,upper)
  erg
}


