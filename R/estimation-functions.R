######################################
# Diese Funktionen dienen dazu Parameter der Regression zu schätzen


######################################
# OLS Funktion
# Bestimmt den OLS Sch?tzer zu eingegebenen Daten data. Die Designmatrix wird f?r ein Polynom vom Grad
# grad erstellt.
# Benutzt wird die systeminterne Funktion lm
# Die R?ckgabewerte sind das Inverse der Designmatrix und die Sch?tzer f?r beta und sigma

OLS <- function(grad, data, nobs){
  #nobs=length(data)
  #time=1:nobs/nobs
  #X=numeric()
  #for(i in 1:(grad+1)){X=matrix(c(X,time^(i-1)),ncol=i)}
  #X.mat=t(X) %*% X
  #inv.X=solve(X.mat)
  #beta.1=inv.X %*% t(X) %*% data
  #sigma=norm(data-X%*%beta,type="2")^2/(nobs-grad-1)

  time=0:(nobs-1)/(nobs-1)
  fit=lm(data~poly(time,degree=grad,raw=T))
  X=model.matrix(fit)
  # setzt die row and coloum names auf numeric()
  rownames(X)=numeric()
  colnames(X)=numeric()
  X.mat=t(X) %*% X
  inv.X=solve(X.mat)

  beta=as.matrix(coefficients(fit))
  rownames(beta)=numeric()
  colnames(beta)=numeric()

  #nobs=length(data)
  #sigma=norm(data-X%*%beta,type="2")^2/(nobs-grad-1)
  sigma=summary(fit)$sigma

  estOut=list(inv.X,beta,sigma)

  estOut
}


###########################################
# fgls Funktion
# f?hrt ein feasible generalised least squares durch
# macht zuerst ein normale OLS Regression indem die OLS Funktion aufgerufen wird. Dann wird die
# Varianz gesch?tzt und damit ein neuer Sch?tzer f?r beta bestimmt
# R?ckgabewerte sind das Inverse der Designmatrix beta bei der fgls Aufgabe und wieder sigma
fgls <- function(grad,data){
  # Parameter sch?tzen f?r die Varianzen
  par.ols <- OLS(grad,data)

  # Varianzmatrix erzeugen
  nobs <- length(data)
  X=numeric()
  for(i in 1:(grad+1)){X=matrix(c(X,time^(i-1)),ncol=i)}
  X.mat=t(X) %*% X
  Var.X <- abs(data-X%*%par.ols[[2]])
  D <- diag(nobs)
  for(i in 1:nobs){D[i,i] <- Var.X[i]}


  # fgls Sch?tzer
  inv.D <- diag(nobs)
  for(i in 1:nobs){inv.D[i,i] <- 1/Var.X[i]}
  inv.D.X <- solve(t(X) %*% inv.D %*% X)
  beta.fgls <- inv.D.X  %*% t(X) %*% inv.D %*% data
  # offenbar ?ndert sich die norm von sigma nicht
  sigma=norm(data-X%*%beta.fgls,type="2")^2/(nobs-grad-1)

  estOut=list(inv.D.X,beta.fgls,sigma)

  estOut
}


#####################################
# AR(1) Funktion
# gibt die Parameter und das inverse der Designmatrix bei einer ar.1 Regression zur?ck.
# Wie funktioniert die Funktion und welche Werte werden benutzt ?
# dabei wird das modell zuerst auf den homoskladistischen Fall zur?ckgezogen
# Was sind die R?ckgabewerte

# time wird selbst erzeugt

# kann den Parameter phi noch nicht automatische auslesen
ar.1 <- function(grad,data, phi){

  nobs=length(data)
  # Trafomatrix bestimmen
  # Upsilon und R haben die selbe Struktur
  R = Upsilon_fun(phi, nobs)[[1]]

  # Wurzel R.trafo aus R.inv berechnen
  # dazu mache ich eine eigenwertzerlegung von inv.R=ev %*% ew %*% ev.1.
  # Dann ist R.trafo = ev %*% sqrt(ew) %*% ev.1
  # R invertieren
  inv.trafo.R = sqrt_inv_mat(R)[[1]]

  # Daten transformieren
  data.trafo = inv.trafo.R %*% data

  # Designmatrix erzeugen und transformieren
  time=1:length(data)/length(data)
  x=time
  X=matrix(data=NA,nrow=length(data),ncol=(grad+1))
  for(j in 1:nobs){
    for(i in 1:(grad+1)){X[j,i]=x[j]^(i-1)}
  }
  X.mat=t(X) %*% X
  inv.X=solve(X.mat)

  X.trafo= inv.trafo.R %*% X
  X.mat.trafo=t(X.trafo) %*% X.trafo
  inv.X.trafo=solve(X.mat.trafo)

  # Regression durchf?hren
  fit=lm(data.trafo~X.trafo-1)

  # anderen Parameter auslesen
  beta=as.matrix(coefficients(fit))
  rownames(beta)=numeric()
  colnames(beta)=numeric()

  sigma=summary(fit)$sigma

  # regression nach der trafo durch anschauen pr?fen
  # time.trafo=inv.trafo.R %*% matrix(time,nrow=length(time))
  # plot(time,data.trafo)
  # curve(beta[1]+beta[2]*x+beta[3]*x^2+beta[4]*x^3, add=T)


  estOut=list(inv.X,beta,sigma,data.trafo,inv.X.trafo,X.trafo)

  estOut
}


