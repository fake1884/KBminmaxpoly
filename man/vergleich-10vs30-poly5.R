# bestimmt ein KB f?r die Differenz von 10KPa und 30 kPa Daten bei einem Polynom vom Grad 5


##########################
# Parameter sch?tzen

grad.5=5
time=1:length(Y.10)/length(Y.10)


# 10 kPa
Y.gls.10 <- gls(Y.10~time+I(time^2)+I(time^3)+I(time^4)+I(time^5),correlation=corAR1())

phi.10=0.8

par.ar.1.10=ar.1(grad.5,Y.10,phi.10)
inv.X.10=par.ar.1.10[[1]]
beta.10=par.ar.1.10[[2]]
sigma.10=par.ar.1.10[[3]]


# 30 kPa
Y.gls.30 <- gls(Y.30~time+I(time^2)+I(time^3)+I(time^4)+I(time^5),correlation=corAR1())

phi.30=0.9

par.ar.1.30=ar.1(grad.5,Y.30,phi.30)
inv.X.30=par.ar.1.30[[1]]
beta.30=par.ar.1.30[[2]]
sigma.30=par.ar.1.30[[3]]

#################################
# plottet die beiden Modelle mit gesch?tzten Parametern in einer Graphik
pdf("man/0-Latex/graphics/Stammzellen-Vergleich/Vergleich-10vs30-poly5.pdf")
plot(time,Y.10,type="l",xlab="Zeit", ylab="Stammzellen")
lines(time,Y.30)
curve(beta.10[1,1]+beta.10[2,1]*x+beta.10[3,1]*x^2+beta.10[4,1]*x^3+
      beta.10[5,1]*x^4+beta.10[6,1]*x^5,col="red", add=T, lwd=2, lty=1)
curve(beta.30[1,1]+beta.30[2,1]*x+beta.30[3,1]*x^2+beta.30[4,1]*x^3+
        beta.30[5,1]*x^4+beta.30[6,1]*x^5,col="green", add=T, lwd=2, lty=1)
dev.off()


#################################
# Konfidenzband der Differenz bestimmen und plotten
# Konfidenzband auf (min(x.1), max(x.1)) bestimmen wobei die Polynomstruktur ber?cksichtigt wird
# da das Modell bereits durch die Verwendung der Funktion ar.1 homoskladistisch gemacht wurde, gibt
# es sonst nichts weiter zu tun.

# Parameter f?r das differenzen-KB bestimmen: Der erste wert ist die Summe der Designmatritzen,
# der zweite Wert die Differenz der betas Y.10 bzw. Y.30 der Modelle, der Dritte der Mittelwert der Varianzen den ich
# wie im F-Test teil des zweiten Kapitels bestimme

# Designmatritzen anpassen
delta.mat=Delta(inv.X.10, inv.X.30)

# Koeffizientenvektor anpassen
beta.diff=Beta.calc(beta.10, beta.30)
beta.10=beta.diff[[1]]
beta.30=beta.diff[[2]]

# feste Werte
alpha=0.05
niter=50
grad=5

# kritischen Wert bestimmen
# der data Vektor wird nur f?r die L?nge ben?tigt, also nehme ich den l?ngeren
par.10vs30=KB.poly.fast(alpha, length(Y.30), grad, niter, delta.mat[[1]], a=0, b=1, ngridpoly = 50)

# Konfidenzband berechnen
plot.bsp.10vs30=plot.KB.vergl(Y.10, Y.30, grad, delta.mat[[1]], beta.10, beta.30, sigma.10, sigma.30,
                              par.10vs30[[1]], ngrid = length(Y.30))


############################################
# Graphik erzeugen
pdf("man/0-Latex/graphics/Stammzellen-Vergleich/Vergleich-10vs30kPa-poly5-KB.pdf")
plot(0,0,xlim=c(0,1),ylim=c(min(plot.bsp.10vs30[[2]]),max(plot.bsp.10vs30[[3]])),
     xlab="relative Zeit", ylab="relatives gr??e des Unterschieds")
lines(c(1,0),c(0,0))
curve(beta.10[1,1]-beta.30[1,1] + (beta.10[2,1]-beta.30[2,1])*x+
        (beta.10[3,1]-beta.30[3,1])*x^2+(beta.10[4,1]-beta.30[4,1])*x^3+
        (beta.10[5,1]-beta.30[5,1])*x^4+(beta.10[6,1]-beta.30[6,1])*x^5,col="red", add=T)
lines(plot.bsp.10vs30[[1]], plot.bsp.10vs30[[2]], col="green")
lines(plot.bsp.10vs30[[1]], plot.bsp.10vs30[[3]], col="green")
dev.off()
