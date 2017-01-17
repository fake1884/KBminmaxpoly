# in diesem R-Script erzeuge ich dieBeispiele für meine BA

#######################
# Daten f?r die Regression sollten mit einem polynomialen Modell der Art AR(1) bestimmt werden,
# damit die Methode ?berzeugt und nicht nur um 0,01 besser ist (im kritischen Wert)

seed.1=100
set.seed(seed.1)

# wahre Werte erzeugen
grad=3
nobs=50
x.raw=c(1:nobs)
x=x.raw/max(x.raw)

X=numeric()
for(i in 1:(grad+1)){X=matrix(c(X,x^(i-1)),ncol=i)}

beta=c(10,5,-4,7)
sigma=1
phi=0.75

Upsilon = Upsilon_fun(phi, length(x.raw))[[1]]

Upsilon=Upsilon*sigma

e=rmvnorm(1,mean=rep(0,length(x.raw)),Upsilon)

y.raw=X %*% beta + t(e)

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

# Phi f?r das Polynom vom Grad eins bestimmen
grad.1=1
time=1:length(y)/length(y)
Y.gls.1 <- gls(y~time,correlation=corAR1())
beta.1=Y.gls.1$coefficients
sigma.1=Y.gls.1$sigma
phi.1=0.7378075

# Regression durchf?hren um die Inverse Designmatrix des transformierten Modells zu bestimmen
fit.hetero.1=ar.1(grad.1,y,phi.1)

# Werte bestimmen
beta.hetero.1=fit.hetero.1[[2]]
sigma.hetero.1=fit.hetero.1[[3]]
inv.X.hetero.1=fit.hetero.1[[1]]
alpha=0.05

# Phi f?r das Polynom vom Grad eins bestimmen
time=1:length(y)/length(y)
Y.gls.3 <- gls(y~time+I(time^2)+I(time^3),correlation=corAR1())
phi.3=-0.9715946

# Regression durchf?hren um die Inverse Designmatrix des transformierten Modells zu bestimmen
fit.hetero.3=ar.1(3,y,phi.3)

# Parameter vom Grad 3 bestimmen
beta.hetero.3=fit.hetero.3[[2]]
sigma.hetero.3=fit.hetero.3[[3]]
inv.X.hetero.3=fit.hetero.3[[1]]
alpha=0.05

# Ergebnisse zeichnen dabei werden die transformierten y-Werte benutzt
pdf("man/0-Latex/graphics/Beispiel/Bsp-Reg-AR.pdf",
    width=10,height=8)

plot(x,y, xlab="relative Zeit", ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
curve(fit.hetero.1[[2]][1]+fit.hetero.1[[2]][2]*x,add=T, cex=2, lwd=3)
curve(fit.hetero.3[[2]][1]+fit.hetero.3[[2]][2]*x+fit.hetero.3[[2]][3]*x^2+fit.hetero.3[[2]][4]*x^3, add=T,
      cex=2, lwd=3, lty="dashed")

legend(x="topleft", legend=c("Grad 1", "Grad 3"),
       col=c("black", "black"),cex=2, lwd=3, lty=c("solid", "dashed"))

dev.off()





##############################
# Konfidenzbaender f?r AR(1) auf min,max f?r polynom

niter=500

# kritischen Wert bestimmen
# alpha, nobs, grad, niter, inv.X, a, b, ngridpoly
par.bsp.poly.hetero=KB.poly.fast(alpha, length(y), 1, niter, inv.X.hetero.1, a=0, b=1, ngridpoly = 50)

# Konfidenzband berechnen
plot.KB.poly.hetero=plot.KB(length(y), 1, inv.X.hetero.1, beta.1, sigma.1, par.bsp.poly.hetero[[1]],
                            ngrid = length(y))

# Ergebnisse zeichnen
pdf("man/0-Latex/graphics/Beispiel/Bsp-KB-poly-AR.pdf",
    width=10,height=8)

plot(0,0,xlim=c(0,1),ylim=c(min(plot.KB.poly.hetero[[2]]),max(plot.KB.poly.hetero[[3]])),
     xlab="relative Zeit", ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
points(x,y, cex=2, lwd=3)
curve(fit.hetero.1[[2]][1]+fit.hetero.1[[2]][2]*x,lty="dashed" ,add=T, cex=2, lwd=3)

lines(plot.KB.poly.hetero[[1]], plot.KB.poly.hetero[[2]], lty=1, cex=2, lwd=3)
lines(plot.KB.poly.hetero[[1]], plot.KB.poly.hetero[[3]], lty=1, cex=2, lwd=3)

legend(x="topleft", legend=c("Polynom AR"), cex=2, lty=1, lwd=3)

dev.off()





#########################################
# Vergleich der Modelle falls AR(1) zugrunde gelegt wird mittels Konfidezbaendern

# Design-Differenzmatrix bestimmen
delta.mat.hetero=Delta(inv.X.hetero.1, inv.X.hetero.3)

# kritischen Wert bestimmen
# alpha, nobs, grad, niter, inv.X, a, b, ngridpoly
par.bsp.vergl.hetero=KB.poly.fast(alpha, length(y), 3, niter, delta.mat.hetero[[1]], a=0, b=1,
                                  ngridpoly = length(y))

# Konfidenzband berechnen
plot.bsp.vergl.hetero=plot.KB.vergl(y, y, 3, delta.mat.hetero[[1]], beta.hetero.1,
                                    beta.hetero.3, sigma.hetero.1, sigma.hetero.3,
                                    par.bsp.vergl.hetero[[1]], ngrid = length(y))

# Ergebnisse zeichnen
pdf("man/0-Latex/graphics/Beispiel/Bsp-KB-poly-hetero-AR.pdf",
    width=10,height=8)

plot(0,0,xlim=c(0,1),ylim=c(min(plot.bsp.vergl.hetero[[2]]),max(plot.bsp.vergl.hetero[[3]])),
     xlab="relative Zeit", ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
lines(c(1,0),c(0,0), cex=2, lwd=3)
curve(fit.hetero.1[[2]][1]-fit.hetero.3[[2]][1]+(fit.hetero.1[[2]][2]-fit.hetero.3[[2]][2])*x-fit.hetero.3[[2]][3]*x^2-fit.hetero.3[[2]][4]*x^3,
      col="black", add=T, cex=2, lwd=3)
lines(plot.bsp.vergl.hetero[[1]], plot.bsp.vergl.hetero[[2]], col="black", cex=2, lwd=3)
lines(plot.bsp.vergl.hetero[[1]], plot.bsp.vergl.hetero[[3]], col="black", cex=2, lwd=3)

dev.off()

########################################################################################
# nur zum auslesen von Werten
par=c( par.bsp.poly.hetero[[1]])
par.vergl=c( par.bsp.vergl.hetero[[1]])

# Werte f?r das Beispiel
kapitel.4=c(beta.hetero.1, sigma.hetero.1, phi.1, par.bsp.poly.hetero, beta.hetero.3, sigma.hetero.3, phi.3,
            par.bsp.vergl.hetero)



