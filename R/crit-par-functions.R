######################################
# KB auf R Funktion
# Diese Funtion bestimmt die kritische Konstante q wie in Kapitel 1.3 angegeben.
# Dazu brauch sie die Daten data, um die Anzahl an Beobachtungen nobs bestimmen. Man muss ihr ansonsten
# noch das Konfidenznivea alpha und den Grad des Polynoms grad ?bergeben. Da zur Bestimmung der Werte
# lower und upper inv.X, beta und sigma gebraucht werden, ?bergebe ich diese auch.
# R?ckgabewert sind points,lower,upper,factor. Dabei ist factor der kritische Wert
KB.R <- function(alpha, data, grad, inv.X){

  # bestimmt die Werte f?r die Berechnung von factor

  # kritischen Wert bestimmen
  nobs=length(data)
  q <-qf(1-alpha,grad+1,nobs-grad-1)
  factor <- sqrt((grad+1)*q)


  erg=list(factor)
  erg
}


######################################
# KB auf R Funktion pr?f funktion
# Diese Funtion bestimmt die kritische Konstante q, um zu pr?fen ob zwei Modelle gleich sind.
# Dazu brauch sie die Daten data, um die Anzahl an Beobachtungen nobs bestimmen. Man muss ihr ansonsten
# noch das Konfidenznivea alpha und den Grad des Polynoms grad ?bergeben. Da zur Bestimmung der Werte
# lower und upper inv.X, beta und sigma gebraucht werden, ?bergebe ich diese auch.
# R?ckgabewert sind points,lower,upper,factor. Dabei ist factor der kritische Wert
KB.R.pruef <- function(alpha, nobs, grad, k){

  # kritischen Wert bestimmen
  q <-qf(1-alpha,k,nobs-grad-1)
  factor <- sqrt(k*q)


  erg=list(factor)
  erg
}



##############################
# KB auf Rechteck Funktion

# Beschreibung nach dem Muster:
# Diese Funktion berechnet ein Konfidenzband auf (0,1) und ist dementsprechend nur f?r normierte Daten
# sinnvoll. Eingabewerte sind das Konfidenznivea alpha, die Daten data, die Anzahl an unabh?nigigen
# Parametern grad (Im Hinterkopf ist die Anwendung auf Polynome), die Anzahl an Iterationen niter
# das inverse der Designmatrix inv.X und Sch?tzer f?r beta und sigma
# Die Funktion sollte genau wie in Kapitel 1.4 funktionieren
# Die R?ckgabewerte sind points,lower,upper,factor wie in der letzten Funktion

# Konfidenzband auf (min(x.1), max(x.1)) bestimmen
KB.minmax <- function(alpha, y, grad, niter, inv.X, a, b){

  # Simulation um den kritischen Wert c zu bestimmen
  # niter sollte mindestens 50.000 sein
  # feste Werte erzeugen
  nobs <- length(y)
  v <- nobs-grad-1
  D <- diag(grad+1)

  # Wurzel P aus inv.X=(X'X)^{-1} berechnen
  # dazu mache ich eine eigenwertzerlegung von inv.X=ev %*% ew %*% ev.1.
  # Dann ist P = ev %*% sqrt(ew) %*% ev.1
  eig <- eigen(inv.X)
  ew <- diag(length(eig$values))
  for(i in 1:length(eig$values)){ew[i,i]=eig$val[i]}
  ew.sq <- sqrt(ew)
  ev <- eig$vec
  ev.1 <- solve(ev)
  #ev %*% ew %*% ev.1 # sollte gleich inv.X sein
  P <- ev %*% ew.sq %*% ev.1
  P.1 <- solve(P)

  # Matrix A bestimmen
  A.temp=matrix(numeric(),ncol=grad+1)
  if(grad>1){
    for(i in 1:(grad-1)){A.rows=rbind((D[,i+1]-b*D[,i]) %*% P.1,(a*D[,i]-D[,i+1]) %*% P.1)
    A.temp=rbind(A.temp,A.rows)}
    A.last <- -D[,1] %*% P.1
    A <- rbind(A.temp,A.last)
  } else {
    A = -D[,1] %*% P.1
  }

  # Simulation beginnt
  temp <- 1
  S <- rep(NA, niter)
  while(temp<(niter+1))
  {
    #rmvnorm(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)),
    #         method=c("eigen", "svd", "chol"), pre0.9_9994 = FALSE)
    N <- mvtnorm::rmvnorm(1,mean=rep(0,grad+1),D)
    sig<-sqrt(rchisq(1,v)/v)
    T.vec <- N/sig
    #solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)
    sol <- quadprog::solve.QP(Dmat=D,dvec=T.vec,Amat=t(A))
    p.T <- norm(sol$solution,type="2")

    sol.minus <- quadprog::solve.QP(Dmat=D,dvec=-T.vec,Amat=t(A))
    p.T.minus <- norm(sol.minus$solution,type="2")

    S[temp]=max(p.T,p.T.minus)

    temp <- temp+1
  }

  # kritische Wert c bestimmen
  factor <- quantile(S,1-alpha)


  erg=list(factor)
  erg
}


##############################
# KB auf Rechteck f?r Polynome im homoskladistischen Fall


# Berechnet den kritischen Wert des Konfidenzbandes im homoskladistischen Fall. Eingabewerte sind wie oben,
# aber dismal ist niter der grad des Polynoms und nicht nur die Anzahl an unabh?ngigen Parametern.
# Die Funktion funktioniert wie in Kapitel 1.5 nur die Berechnung von den Maxima ist besonders. Daf?r
# sehe die Kommentare im Programmcode.
# R?ckgabewerte wie immer.

KB.poly <- function(alpha, nobs, grad, niter, inv.X, a, b){


  # Diese Funktionen braucht man sp?ter in der Berechnung, um das Maximum zu bestimmen
  # x.fun berechnet den Wert an x und x.fun.prime an der Ableitung, die f?r Polynome leicht zu
  # bestimmen ist
  # grad+1, da wir grad des polynoms + konstante brauchen

  x_fun <- function(x, grad) {
    vec=rep(NA, grad+1)
    for(i in 1:(grad+1)){vec[i]=x^(i-1)}
    return(vec)
  }

  x_fun_prime <- function(x, grad) {
    vec=rep(NA, grad+1)
    vec[1]=0
    for(i in 1:grad){vec[i+1]=i*x^(i-1)}
    return(vec)
  }

  g.fun.prime <- function (x, T.vec, grad) {(t(x_fun_prime(x, grad)) %*% t(T.vec)) * (t(x_fun(x, grad)) %*% inv.X %*% x_fun(x, grad)) - t(x_fun(x, grad))%*%t(T.vec) * t(x_fun_prime(x, grad))%*%inv.X%*%x_fun(x, grad)}

  g.fun <- function (x, T.vec, grad) {(t(x_fun(x, grad))%*%t(T.vec))/(sqrt(t(x_fun(x, grad))%*%inv.X%*%x_fun(x, grad)))}

  S.fun = function(v, ngridpoly, xseq, mod, ss){

    # Wert simulieren
    T.vec <- mvtnorm::rmvnorm(n=1,mean=rep(0,grad+1),sigma=inv.X) / sqrt(rchisq(n=1,df=v)/v)

    # Berechnug der Maxima
    # orientiert sich an der funktion uniroot.all aus dem rootSolve Package
    # uniroot.all does that by first subdividing the interval into small sections and, for all sections
    # where the function value changes sign, invoking uniroot to locate the root.

    g.lapply=function(x){g.fun.prime(x, T.vec, grad)}
    mod=sapply(xseq, g.lapply)

    # ist mod==0 liegt schonmal eine Wurzel vor
    roots.1 <- xseq[which(mod == 0)]

    # multipliziert aufeinanderfolgende Funktionswerte
    ss <- mod[1:ngridpoly] * mod[2:(ngridpoly + 1)]

    # ist ss[i]<0, so liegt ein Vorzeichenwechsel vor und zwischen mod[i] und mod[i+1]
    # liegt ein Nullstelle -> ii speichert die Inidizes der Werte von ss, die kleiner Null sind
    ii <- which(ss < 0)

    # loop ?ber alle indizes in ii. Bei jedem Durchlauf Aufruf der uniroot-Funktion, die Wurzeln im
    # Eindimensionalen findet. Alle diese Wurzeln werden in all.roots gespeichert.
    # wrap-around function, da uniroot nur eindimensional funktionen vertr?gt
    roots.2=rep(NA, length(ii))
    temp.1=1
    for (i in ii){roots.2[temp.1]= (uniroot(function(x)(g.fun.prime(x,T.vec, grad)), lower = xseq[i], upper = xseq[i + 1])$root)
    temp.1=temp.1+1}
    all.roots=c(roots.1,roots.2)

    # als n?chstes soll g.fun(i) f?r i in all.roots gefunden werden
    # Fallunterscheidung, ob Wurzeln gefunden wurden
    if(is.na(all.roots[1])==T){erg=max(abs(g.fun(a, T.vec, grad)),abs(g.fun(b, T.vec, grad)))}
    else {
      # all.g speichert die Funktionswerte von g.fun an den Nullstellen von g.fun.prime
      all.g <- rep(NA, length(all.roots))
      temp=c(1:length(all.roots))

      g.fun.apply=function(x){g.fun(x, T.vec, grad)}
      all.g=sapply(temp, g.fun.apply)

      erg=max(abs(g.fun(a, T.vec, grad)),abs(g.fun(b, T.vec, grad)),abs(all.g))
    }

    erg
  }


  # feste Werte erzeugen
  v=nobs-grad-1
  ngridpoly = 100 # Feinheit des Grids festlegen, auf dem wir nachher die wurzeln von g.fun.prime suchen
  xseq <- seq(a, b, len = ngridpoly + 1) # subdividing the interval [a,b] in ngrid+1 parts
  mod = rep(NA,(ngridpoly+1)) # mod speichert die Funktionswerte auf [a,b] mit Abstand ngrid
  ss=rep(NA,(ngridpoly+1)) # multipliziert aufeinander folgende Funktionswerte
  S <- rep(NA, niter)  # lehrer Vektor f?r die Simulationsergebnisse

  # Simulation durchf?heren
  for(i in 1:niter)
  {
    S[i]=S.fun(v, ngridpoly, xseq, mod, ss)
  }

  # kritischen Wert bestimmen
  factor <- quantile(S,1-alpha)


  erg=list(factor)
  erg
}

#################################################
# Diese Funktion bestimmt das Maximum der Funktion g auf einem Grid mit Feinheit ngridpoly auf [0,1]
# indem die Funktionswerte von jedem Wert bestimmt werden.
# Dies müsste schneller, als der letzte Ansatz funktionieren
KB.poly.fast <- function(alpha, nobs, grad, niter, inv.X, a, b, ngridpoly){


  # von dieser Funktion m?chte man das Maximum bestimmen

  x_fun <- function(x, grad) {
    vec=rep(NA, grad+1)
    for(i in 1:(grad+1)){vec[i]=x^(i-1)}
    return(vec)
  }

  x_fun_prime <- function(x, grad) {
    vec=rep(NA, grad+1)
    vec[1]=0
    for(i in 1:grad){vec[i+1]=i*x^(i-1)}
    return(vec)
  }

  g.fun <- function (x, T.vec, grad) {(t(x_fun(x, grad))%*%t(T.vec))/(sqrt(t(x_fun(x, grad))%*%inv.X%*%x_fun(x, grad)))}

  S.fun = function(v, values, xseq){

    # Wert simulieren
    T.vec <- mvtnorm::rmvnorm(n=1,mean=rep(0,grad+1),sigma=inv.X) / sqrt(rchisq(n=1,df=v)/v)

    # Berechnug der Maxima
    # Bestimmt einfach den Wert von g.fun auf einem Grid auf [0,1] mit Feinheit ngridpoly

    g.lapply=function(x){g.fun(x, T.vec, grad)}
    values=sapply(xseq, g.lapply)

    erg=max(abs(values))

    erg
  }


  # feste Werte erzeugen
  v=nobs-grad-1
  xseq <- seq(a, b, len = ngridpoly + 1) # subdividing the interval [a,b] in ngrid+1 parts
  values = rep(NA,(ngridpoly+1)) # mod speichert die Funktionswerte auf [a,b] mit Abstand ngrid
  S <- rep(NA, niter)  # lehrer Vektor f?r die Simulationsergebnisse

  # Simulation durchf?heren
  for(i in 1:niter){
    S[i]=S.fun(v,values, xseq)
  }

  # kritischen Wert bestimmen
  factor <- quantile(S,1-alpha)


  erg=list(factor)
  erg
}



