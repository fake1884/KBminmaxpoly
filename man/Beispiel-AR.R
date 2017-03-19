# in diesem R-Script erzeuge ich dieBeispiele für meine BA

#######################
# Daten f?r die Regression sollten mit einem polynomialen Modell der Art AR(1) bestimmt werden,
# damit die Methode ?berzeugt und nicht nur um 0,01 besser ist (im kritischen Wert)

seed.1=100
set.seed(seed.1)

# wahre Werte erzeugen

# feste Werte initialisieren
grad=3
grad.1=3
grad.3=4
nobs=50
beta.true=c(10,5,-4,7)
sigma.true=1 #0.007545373
phi.true=0.001 #0.8225374  # bestimmt die korrelation
x.raw=c(0:(nobs-1))
x=x.raw/max(x.raw)

X=matrix(data=NA,nrow=nobs,ncol=(grad+1))
for(j in 1:nobs){
  for(i in 1:(grad+1)){X[j,i]=x[j]^(i-1)}
}
X.mat=t(X) %*% X
X.mat.inv.1=solve(X.mat)

X.3=matrix(data=NA,nrow=nobs,ncol=(grad.3+1))
for(j in 1:nobs){
  for(i in 1:(grad.3+1)){X.3[j,i]=x[j]^(i-1)}
}
X.mat.3=t(X.3) %*% X.3
X.mat.inv.3=solve(X.mat.3)

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
alpha=0.05
ngrid=nobs


Upsilon = Upsilon_fun(phi.true, length(x.raw))[[1]]

Upsilon=Upsilon*sigma.true

e=rmvnorm(1,mean=rep(0,length(x.raw)),Upsilon)

y.raw=X %*% beta.true + t(e)

# plot der Daten
pdf("man/0-Latex/graphics/Beispiel/data-raw-AR.pdf",
    width=10,height=8)

plot(x.raw,y.raw, xlab="Zeit", ylab="Wachstum", pch=1, cex=2, lwd=3, cex.axis=2, cex.lab=2)

dev.off()




############################

# Daten normalisieren
y=y.raw/max(y.raw)


#######################
# Regression falls AR(1) als Modell zugrunde gelegt wird

# Phi f?r das Polynom vom Grad drei bestimmen
grad.1=3
Y.gls.1 <- gls(y~time+time.2+time.3,correlation=corAR1())
beta.1=Y.gls.1$coefficients
sigma.1=Y.gls.1$sigma


# Phi f?r das Polynom vom Grad vier bestimmen
grad.3=4
Y.gls.3 <- gls(y~time+time.2+time.3+time.4,correlation=corAR1())
beta.3=Y.gls.3$coefficients
sigma.3=Y.gls.3$sigma

# Beide Regressionsmodelle mit den Daten in einer Graphik einzeichnen
pdf("man/0-Latex/graphics/Beispiel/Bsp-Reg-AR.pdf",
    width=10,height=8)

plot(x,y, xlab="relative Zeit", ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
curve(beta.1[1]+beta.1[2]*x+beta.1[3]*x^2+beta.1[4]*x^3,add=T, cex=2, lwd=3)
curve(beta.3[1]+beta.3[2]*x+beta.3[3]*x^2+beta.3[4]*x^3+beta.3[5]*x^4, add=T,
      cex=2, lwd=3, lty="dashed")

legend(x="topleft", legend=c("Grad 3", "Grad 4"),
       col=c("black", "black"),cex=2, lwd=3, lty=c("solid", "dashed"))

dev.off()





##############################
# Konfidenzbaender f?r AR(1) auf min,max f?r polynom

niter=5000

# kritischen Wert bestimmen
# alpha, nobs, grad, niter, inv.X, a, b, ngridpoly
par.bsp.poly.1=KB.poly.fast(alpha, length(y), grad = 3, niter, X.mat.inv.1, a=0, b=1, ngridpoly = 100)

# Konfidenzband berechnen
plot.KB.poly.1=plot.KB(length(y), grad = 3, X.mat.inv.1, beta.1, sigma.1, par.bsp.poly.1[[1]],
                            ngrid = length(y))

# alpha, nobs, grad, niter, inv.X, a, b, ngridpoly
par.bsp.poly.3=KB.poly.fast(alpha, length(y), grad = 4, niter, X.mat.inv.3, a=0, b=1, ngridpoly = 100)

# Konfidenzband berechnen
plot.KB.poly.3=plot.KB(length(y), grad = 4, X.mat.inv.3, beta.3, sigma.3, par.bsp.poly.1[[1]],
                            ngrid = length(y))

# Ergebnisse zeichnen
pdf("man/0-Latex/graphics/Beispiel/Bsp-KB-poly-AR.pdf",
    width=10,height=8)

plot(0,0,xlim=c(0,1),ylim=c(min(plot.KB.poly.hetero[[2]]),max(plot.KB.poly.hetero[[3]])),
     xlab="relative Zeit", ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
points(x,y, cex=2, lwd=3)

curve(beta.1[1]+beta.1[2]*x+beta.1[3]*x^2+beta.1[4]*x^3,add=T, cex=2, lwd=3)
curve(beta.3[1]+beta.3[2]*x+beta.3[3]*x^2+beta.3[4]*x^3+beta.3[5]*x^4, add=T,
      cex=2, lwd=3, lty="dashed")

lines(plot.KB.poly.1[[1]], plot.KB.poly.1[[2]], lty=1, cex=2, lwd=3)
lines(plot.KB.poly.1[[1]], plot.KB.poly.1[[3]], lty=1, cex=2, lwd=3)

lines(plot.KB.poly.3[[1]], plot.KB.poly.3[[2]], lty="dashed", cex=2, lwd=3)
lines(plot.KB.poly.3[[1]], plot.KB.poly.3[[3]], lty="dashed", cex=2, lwd=3)

legend(x="topleft", legend=c("Grad 3", "Grad 4"),
       col=c("black","black"),cex=2, lty=c(1,2), lwd=3)

dev.off()





#########################################
# Vergleich der Modelle falls AR(1) zugrunde gelegt wird mittels Konfidezbaendern

# Design-Differenzmatrix bestimmen
delta.mat.hetero=Delta(X.mat.inv.1, X.mat.inv.3)

# kritischen Wert bestimmen
# alpha, nobs, grad, niter, inv.X, a, b, ngridpoly
par.bsp.vergl.hetero=KB.poly.fast(alpha, length(y), grad = 4, niter, delta.mat.hetero[[1]], a=0, b=1,
                                  ngridpoly = length(y))

# Konfidenzband berechnen
plot.bsp.vergl.hetero=plot.KB.vergl(y, y, grad = 4, delta.mat.hetero[[1]], beta.1,
                                    beta.3, sigma.1, sigma.3,
                                    par.bsp.vergl.hetero[[1]], ngrid = length(y))

# Ergebnisse zeichnen
pdf("man/0-Latex/graphics/Beispiel/Bsp-KB-poly-hetero-AR.pdf",
    width=10,height=8)

plot(0,0,xlim=c(0,1),ylim=c(min(plot.bsp.vergl.hetero[[2]]),max(plot.bsp.vergl.hetero[[3]])),
     xlab="relative Zeit", ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
lines(c(1,0),c(0,0), cex=2, lwd=3)

beta.vergl=c(beta.1,0)-beta.3
lines(time, X.3%*%beta.vergl, cex=2, lwd=3)

lines(plot.bsp.vergl.hetero[[1]], plot.bsp.vergl.hetero[[2]], col="black", cex=2, lwd=3)
lines(plot.bsp.vergl.hetero[[1]], plot.bsp.vergl.hetero[[3]], col="black", cex=2, lwd=3)

dev.off()

########################################################################################
# nur zum auslesen von Werten
par=c( par.bsp.poly.hetero[[1]])
par.vergl=c( par.bsp.vergl.hetero[[1]])

# Werte für das Beispiel
#kapitel.4=c(beta.hetero.1, sigma.hetero.1, phi.1, par.bsp.poly.hetero, beta.hetero.3, sigma.hetero.3, phi.3,
#            par.bsp.vergl.hetero)



