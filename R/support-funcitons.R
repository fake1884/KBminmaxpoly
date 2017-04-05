
##############################
# Differenzendesignmatrix bestimmen

# Diese Funktion bestimmt die Differenzendesignmatrix der eingegebenen Matritzen inv.X.1 und inv.X.2
# Das Problem kann sein, dass die Matritzen verschiedene Dimensionen haben.
# R?ckgabewerte wie oben.

Delta <- function(inv.X.1, inv.X.2){

  while(dim(inv.X.1)[1]<dim(inv.X.2)[1])
  {
    inv.X.1=rbind(cbind(inv.X.1,rep(0,dim(inv.X.1)[1])),rep(0,dim(inv.X.1)[1]+1))
  }
  while(dim(inv.X.2)[1]<dim(inv.X.1)[1])
  {
    inv.X.2=rbind(cbind(inv.X.2,rep(0,dim(inv.X.1)[1])),rep(0,dim(inv.X.1)[1]+1))
  }

  delta.mat=inv.X.1+inv.X.2

  erg=list(delta.mat)
  erg
}




##############################
# Differenzenbetavektor bestimmen

# Diese Funktion bestimmt die Differenzendesignmatrix der eingegebenen Matritzen inv.X.1 und inv.X.2
# Das Problem kann sein, dass die Matritzen verschiedene Dimensionen haben.
# R?ckgabewerte wie oben.

Beta.calc <- function(beta.1, beta.2){

  # betas auf die richtige L?nge bringen
  while(length(beta.1)<length(beta.2))
  {
    beta.1=as.matrix(c(beta.1,0))
  }
  while(length(beta.2)<length(beta.1))
  {
    beta.2=as.matrix(c(beta.2,0))
  }


  erg=list(beta.1, beta.2)
  erg
}



##############################
# I_tilde bestimmen

# Diese Funktion bestimmt die Matrix I_tilde=(0,I_k) die beim pr?fen von Regressionsmodellen gebraucht
# wird

I_tilde <- function(k, p){

  # Zuerst eine Diagonalmatrix erstellen
  I.k = diag(k)

  # Dann die Nullmatrix erstellen
  I.0=matrix(rep(0,(p+1-k)*k),nrow=k,ncol=(p+1-k))

  # und beide zusammenf?gen
  I.tilde = cbind(I.0,I.k)


  erg=list(I.tilde)
  erg
}



##############################
# Upsilon bestimmen

# Diese Funktion bestimmt die Upsilon-Matrix, die die Kovarianzstruktur des Regressionsmodells darstellt

Upsilon_fun <- function(phi, dimension){

  # Zuerst eine Diagonalmatrix erstellen
  Upsilon = diag(dimension)

  # Dann die beiden Nebendiagonalen auff?llen
  for (j in 1:(dimension-1)){
    for (i in (1+j):dimension)
    {
      Upsilon[i,j]=phi^(i-j)
    }
  }
  for (j in 1:(dimension-1)){
    for (i in (1+j):dimension)
    {
      Upsilon[j,i]=phi^(i-j)
    }
  }


  erg=list(Upsilon)
  erg
}


###################################################
# berechnet die V Matrix aus den Überprüfungsberechnungen

V = function(I.tilde, inv.X){

  return(I.tilde %*% inv.X %*% t(I.tilde))
  }


########################################################
# berechnet zu einer Matrix R die Matirx R^{-1/2}
# R^{-1} = inv.R
# R^{-1/2} = sqrt.inv.R
# dazu mache ich eine eigenwertzerlegung von inv.R=ev %*% ew %*% ev.1.
# Dann ist sqrt.inv.R = ev %*% sqrt(ew) %*% ev.1

sqrt_inv_mat <- function(R){

  eig <- eigen(R)
  ew <- diag(length(eig$values))
  for(i in 1:length(eig$values)){ew[i,i]=eig$val[i]}
  ew.sq <- sqrt(ew)
  ev <- eig$vec
  #ev %*% ew.sq %*% ev.1 # sollte gleich inv.X sein
  ev.1 <- solve(ev)
  R.trafo <- ev %*% ew.sq %*% ev.1
  # R invertieren
  inv.trafo.R=solve(R.trafo)

  erg=list(inv.trafo.R)
  erg
}
