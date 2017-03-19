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

I.mat = diag(length(x.raw))

e=rmvnorm(1,mean=rep(0,length(x.raw)), sigma=sigma*I.mat)

y.raw=X %*% beta + t(e)

# plot der Daten
pdf("man/0-Latex/graphics/Beispiel/data-raw-R.pdf",
    width=10,height=8)

plot(x.raw,y.raw, xlab="Zeit", ylab="Wachstum", pch=1, cex=2, lwd=3, cex.axis=2, cex.lab=2)

dev.off()




############################

# Daten normalisieren
y=y.raw/max(y.raw)



##########################
# lineare Regression und plot der Daten mit regression
grad.1=3
fit.1=OLS(grad.1, y, length(y))
inv.X.1=fit.1[[1]]
beta.1=fit.1[[2]]
sigma.1=fit.1[[3]]

beta.traf=beta.1 * max(y.raw)
sigma.traf=sigma.1 * max(y.raw)

pdf("man/0-Latex/graphics/Beispiel/regression-gerade.pdf",
    width=10,height=8)

plot(x,y,xlab="relative Zeit", ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
curve(fit.1[[2]][1]+fit.1[[2]][2]*x+fit.1[[2]][3]*x^2+fit.1[[2]][4]*x^3, add=T, cex=2, lwd=3)

dev.off()



###############################
# punktweise vs gleichm??iges Konfidenzband

# Werte des Konfidenzbandes bestimmen
alpha=0.05

# kritischen Wert berechnen
par.bsp.R=KB.R(alpha, y, grad.1, inv.X.1)

# Konfidenzband bestimmen
plot.KB.R=plot.KB(length(y), grad.1, inv.X.1, beta.1, sigma.1, par.bsp.R[[1]], ngrid = length(y))

# punktweises Konfidenzband kritischer Wert
par.bsp.punkt=qt(1-alpha/2, length(y)-grad.1-1)

# punktweise Konfidenzband bestimmen
plot.KB.punkt=plot.KB(length(y), grad.1, inv.X.1, beta.1, sigma.1, par.bsp.punkt, ngrid = length(y))

# Ergebnisse zeichnen
pdf("man/0-Latex/graphics/Beispiel/punkt-vs-gleich.pdf",
    width=10,height=8)

plot(0,0,xlim=c(0,1),ylim=c(min(plot.KB.R[[2]]),max(plot.KB.R[[3]])), xlab="relative Zeit",
     ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
points(x,y, cex=2, lwd=3)
curve(fit.1[[2]][1]+fit.1[[2]][2]*x+fit.1[[2]][3]*x^2+fit.1[[2]][4]*x^3, add=T, cex=2, lwd=3)
lines(plot.KB.R[[1]], plot.KB.R[[2]], lty=1, cex=2, lwd=3)
lines(plot.KB.R[[1]], plot.KB.R[[3]], lty=1, cex=2, lwd=3)
lines(plot.KB.punkt[[1]], plot.KB.punkt[[2]], lty=2, cex=2, lwd=3)
lines(plot.KB.punkt[[1]], plot.KB.punkt[[3]], lty=2, cex=2, lwd=3)

legend(x="topleft", legend=c("gleichmäßig","punktweise"),
       col=c("black","black"),cex=2, lty=c(1,2), lwd=3)

dev.off()


#############################
# Konfidenzband auf ganz R

# Werte des Konfidenzbandes bestimmen
alpha=0.05

# kritischen Wert berechnen
par.bsp.R=KB.R(alpha, y, grad.1, inv.X.1)

# Konfidenzband bestimmen
plot.KB.R=plot.KB(length(y), grad.1, inv.X.1, beta.1, sigma.1, par.bsp.R[[1]], ngrid = length(y))


# Ergebnisse zeichnen
pdf("man/0-Latex/graphics/Beispiel/Bsp-KB-R.pdf",
    width=10,height=8)

plot(0,0,xlim=c(0,1),ylim=c(min(plot.KB.R[[2]]),max(plot.KB.R[[3]])), xlab="relative Zeit",
     ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
points(x,y, cex=2, lwd=3)
curve(fit.1[[2]][1]+fit.1[[2]][2]*x+fit.1[[2]][3]*x^2+fit.1[[2]][4]*x^3, add=T, cex=2, lwd=3)
lines(plot.KB.R[[1]], plot.KB.R[[2]], lty=1, cex=2, lwd=3)
lines(plot.KB.R[[1]], plot.KB.R[[3]], lty=1, cex=2, lwd=3)

legend(x="topleft", legend=c("R"),
       col=c("black"),cex=2, lwd=3, lty=c("solid"))

dev.off()




###########################
# Konfidenzband auf min,max

# Simulation beginnt
set.seed(4)
niter=5000

# kritischen Wert bestimmen
par.bsp.minmax=KB.minmax(alpha, y, grad.1, niter, inv.X.1, a=0, b=1)

# Konfidenzband berechnen
plot.KB.minmax=plot.KB(length(y), grad.1, inv.X.1, beta.1, sigma.1, par.bsp.minmax[[1]], ngrid = length(y))

# Ergebnisse zeichnen
pdf("man/0-Latex/graphics/Beispiel/Bsp-KB-minmax.pdf",
    width=10,height=8)

plot(0,0,xlim=c(0,1),ylim=c(min(plot.KB.R[[2]]),max(plot.KB.R[[3]])), xlab="relative Zeit",
     ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
points(x,y, cex=2, lwd=3)
curve(fit.1[[2]][1]+fit.1[[2]][2]*x+fit.1[[2]][3]*x^2+fit.1[[2]][4]*x^3, add=T, cex=2, lwd=3)
lines(plot.KB.R[[1]], plot.KB.R[[2]], lty="solid", cex=2, lwd=3)
lines(plot.KB.R[[1]], plot.KB.R[[3]], lty="solid", cex=2, lwd=3)
lines(plot.KB.minmax[[1]], plot.KB.minmax[[2]], lty="dotted", cex=2, lwd=3)
lines(plot.KB.minmax[[1]], plot.KB.minmax[[3]], lty="dotted", cex=2, lwd=3)

legend(x="topleft", legend=c("R", "[0,1]"),
       col=c("black", "black"),cex=2, lty=c("solid", "dotted"), lwd = 3)

dev.off()



############################
# Konfidenzband auf min,max für polynome

# kritischen Wert bestimmen
# alpha, nobs, grad, niter, inv.X, a, b, ngridpoly
par.bsp.poly=KB.poly.fast(alpha, length(y), grad.1, niter, inv.X.1, a=0, b=1, ngridpoly = 100)

# Konfidenzband berechnen
plot.KB.poly=plot.KB(length(y), grad.1, inv.X.1, beta.1, sigma.1, par.bsp.poly[[1]], ngrid = length(y))

# Ergebnisse zeichnen
pdf("man/0-Latex/graphics/Beispiel/Bsp-KB-poly.pdf",
    width=10,height=8)

plot(0,0,xlim=c(0,1),ylim=c(min(plot.KB.R[[2]]),max(plot.KB.R[[3]])), xlab="relative Zeit",
     ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
points(x,y, cex=2, lwd=3)
curve(fit.1[[2]][1]+fit.1[[2]][2]*x+fit.1[[2]][3]*x^2+fit.1[[2]][4]*x^3, add=T, cex=2, lwd=3)
lines(plot.KB.R[[1]], plot.KB.R[[2]], lty="solid", cex=2, lwd=3)
lines(plot.KB.R[[1]], plot.KB.R[[3]], lty="solid", cex=2, lwd=3)
lines(plot.KB.minmax[[1]], plot.KB.minmax[[2]], lty="dotted", cex=2, lwd=3)
lines(plot.KB.minmax[[1]], plot.KB.minmax[[3]], lty="dotted", cex=2, lwd=3)
lines(plot.KB.poly[[1]], plot.KB.poly[[2]], lty="dashed", cex=2, lwd=3)
lines(plot.KB.poly[[1]], plot.KB.poly[[3]], lty="dashed", cex=2, lwd=3)

legend(x="topleft", legend=c("R", "[0,1]", "Polynom"),
       col=c("black", "black", "black"),cex=2, lwd=3, lty=c("solid", "dotted", "dashed"))

dev.off()



#############################
# Unterschied zwischen Regression von Grad 3 und 4

# Regression vom Grad 3
grad.3=4
fit.3=OLS(grad.3, y, length(y))
inv.X.3=fit.3[[1]]
beta.3=fit.3[[2]]
sigma.3=fit.3[[3]]

# Plot der beiden Regressionsmodellen
pdf("man/0-Latex/graphics/Beispiel/Bsp-beide-in-einem-plot.pdf",
    width=10,height=8)

plot(x,y, xlab="relative Zeit", ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
curve(fit.1[[2]][1]+fit.1[[2]][2]*x+fit.1[[2]][3]*x^2+fit.1[[2]][4]*x^3, add=T, cex=2, lwd=3)
curve(fit.3[[2]][1]+fit.3[[2]][2]*x+fit.3[[2]][3]*x^2+fit.3[[2]][4]*x^3+fit.3[[2]][5]*x^4,
      add=T, lty="dashed", cex=2, lwd=3)

legend(x="topleft", legend=c("Grad 3", "Grad 4"),
       col=c("black", "black"),cex=2, lwd=3, lty=c("solid", "dashed"))

dev.off()



###############################
# F-Test

# Werte erzeugen
alpha=0.05
data.1=y
data.2=y

# Test durchf?hren
erg=f.test(alpha, grad.1, grad.3, data.1, data.2)
teststat=erg[1]
q=erg[2]




############################
# Vergleich mittels Konfidezbaendern

# Parameter vom Grad 3 bestimmen
alpha=0.05

# Design-Differenzmatrix bestimmen
delta.mat=Delta(inv.X.1, inv.X.3)

# kritischen Wert bestimmen
# alpha, nobs, grad, niter, inv.X, a, b, ngridpoly
par.bsp.vergl=KB.poly.fast(alpha, length(y), max(grad.1, grad.3), niter, delta.mat[[1]], a=0, b=1,
                           ngridpoly = 50)

# Konfidenzband berechnen
plot.bsp.vergl=plot.KB.vergl(y, y, grad.3, delta.mat[[1]], beta.1, beta.3, sigma.1, sigma.3,
                             par.bsp.vergl[[1]], ngrid = length(y))

# Ergebnisse zeichnen
pdf("man/0-Latex/graphics/Beispiel/Bsp-KB-poly-hetero.pdf",
    width=10,height=8)

plot(0,0,xlim=c(0,1),ylim=c(min(plot.bsp.vergl[[2]]),max(plot.bsp.vergl[[3]])), xlab="relative Zeit",
     ylab="relatives Wachstum", cex=2, lwd=3, cex.axis=2, cex.lab=2)
lines(c(1,0),c(0,0), cex=2, lwd=3)
curve(fit.1[[2]][1]-fit.3[[2]][1]+(fit.1[[2]][2]-fit.3[[2]][2])*x+(fit.1[[2]][3]-fit.3[[2]][3])*x^2+
        (fit.1[[2]][4]-fit.3[[2]][4])*x^3-fit.3[[2]][5]*x^4
      , add=T, cex=2, lwd=3)
lines(plot.bsp.vergl[[1]], plot.bsp.vergl[[2]], lty=1, cex=2, lwd=3)
lines(plot.bsp.vergl[[1]], plot.bsp.vergl[[3]], lty=1, cex=2, lwd=3)

dev.off()


########################################################################################
# nur zum auslesen von Werten
par=c(par.bsp.R[[1]], par.bsp.minmax[[1]], par.bsp.poly[[1]])
par.vergl=c(par.bsp.vergl[[1]])

# Werte f?r das Beispiel
simulation=c(seed.1, nobs, beta, sigma, grad, max(y.raw))
kapitel.1=c(grad.1, beta.1, sigma.1, par.bsp.R, par.bsp.minmax, par.bsp.poly)
kapitel.2=c(grad.3, beta.3, sigma.3, erg, teststat, par.bsp.vergl)




