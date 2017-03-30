# bestimmt ein KB f?r die Differenz von 10KPa und 30 kPa Daten bei einem Polynom vom Grad 5


##########################
# Parameter sch?tzen

grad.5=5
time=1:length(Y.10)/length(Y.10)
nobs=length(Y.10)

# 10 kPa
time.2=time^2
time.3=time^3
time.4=time^4
time.5=time^5
Y.gls.10 <- gls(Y.10~time+time.2+time.3+time.4+time.5,correlation=corAR1())

beta.10=Y.gls.10$coeff
sigma.10=Y.gls.10$sigma

# do the model generation
X.10=matrix(data=NA,nrow=nobs,ncol=(grad.5+1))
for(j in 1:nobs){
  for(i in 1:(grad.5+1)){X.10[j,i]=time[j]^(i-1)}
}
X.mat.10=t(X.10) %*% X.10
inv.X.10=solve(X.mat.10)


# 30 kPa
Y.gls.30 <- gls(Y.30~time+time.2+time.3+time.4+time.5,correlation=corAR1())

beta.30=Y.gls.30$coeff
sigma.30=Y.gls.30$sigma

# do the model generation
X.30=matrix(data=NA,nrow=nobs,ncol=(grad.5+1))
for(j in 1:nobs){
  for(i in 1:(grad.5+1)){X.30[j,i]=time[j]^(i-1)}
}
X.mat.30=t(X.30) %*% X.30
inv.X.30=solve(X.mat.30)


#################################
# plottet die beiden Modelle mit gesch?tzten Parametern in einer Graphik
pdf("man/0-Latex/graphics/Stammzellen-Vergleich/Vergleich-10vs30-poly5.pdf", width = 10, height = 8)
par(mar=c(5.1,5.1,4.1,2.1))
plot(time,Y.10,type="l",xlab="Zeit", ylab="Stammzellen", cex=2, lwd=3, cex.axis=2, cex.lab=2)
lines(time,Y.30, lwd=3)
curve(beta.10[1]+beta.10[2]*x+beta.10[3]*x^2+beta.10[4]*x^3+
      beta.10[5]*x^4+beta.10[6]*x^5,col="black", add=T, lwd=2, lty="solid")
curve(beta.30[1]+beta.30[2]*x+beta.30[3]*x^2+beta.30[4]*x^3+
        beta.30[5]*x^4+beta.30[6]*x^5,col="black", add=T, lwd=2, lty="dashed")
legend(x="topleft", legend=c("10kPa", "30kPa"),
       col=c("black", "black"),cex=2, lwd=3, lty=c("solid", "dashed"))
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
niter=1000
grad=5

# kritischen Wert bestimmen
# der data Vektor wird nur f?r die L?nge ben?tigt, also nehme ich den l?ngeren
par.10vs30=KB.poly.fast(alpha, length(Y.30), grad, niter, delta.mat[[1]], a=0, b=1, ngridpoly = 100)

# Konfidenzband berechnen
plot.bsp.10vs30=plot.KB.vergl(Y.10, Y.30, grad, delta.mat[[1]], beta.10, beta.30, sigma.10, sigma.30,
                              par.10vs30[[1]], ngrid = length(Y.30))


############################################
# Graphik erzeugen
pdf("man/0-Latex/graphics/Stammzellen-Vergleich/Vergleich-10vs30kPa-poly5-KB.pdf", width = 10, height = 8)
par(mar=c(5.1,5.1,4.1,2.1))
plot(0,0,xlim=c(0,1),ylim=c(min(plot.bsp.10vs30[[2]]),max(plot.bsp.10vs30[[3]])),
     xlab="relative Zeit", ylab="relatives Größe des Unterschieds", cex=2, lwd=3, cex.axis=2, cex.lab=2)
lines(c(1,0),c(0,0), lwd=3)
curve(beta.10[1]-beta.30[1] + (beta.10[2]-beta.30[2])*x+
        (beta.10[3]-beta.30[3])*x^2+(beta.10[4]-beta.30[4])*x^3+
        (beta.10[5]-beta.30[5])*x^4+(beta.10[6]-beta.30[6])*x^5,col="black", add=T, lwd=3)
lines(plot.bsp.10vs30[[1]], plot.bsp.10vs30[[2]], col="black", lwd=3)
lines(plot.bsp.10vs30[[1]], plot.bsp.10vs30[[3]], col="black", lwd=3)
dev.off()
