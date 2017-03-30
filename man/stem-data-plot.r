# Erzeugt die nackten Plots der Daten für die Arbeit

nobs.10=length(Y.10)
time.10.raw=c(0:(nobs.10-1))
time.10=time.10.raw/max(time.10.raw)

pdf("man/0-Latex/graphics/Stammzellen-10kPa/10kPa-data.pdf", width = 10, height = 8)
par(mar=c(5.1,5.1,4.1,2.1))
plot(time.10, Y.10, type="l", xlab="relative Zeit", ylab="relatives Wachstum",
     cex=2, lwd=3, cex.axis=2, cex.lab=2)
dev.off()

nobs.30=length(Y.30)
time.30.raw=c(0:(nobs.30-1))
time.30=time.30.raw/max(time.30.raw)

pdf("man/0-Latex/graphics/Stammzellen-30kPa/30kPa-data.pdf",  width = 10, height = 8)
par(mar=c(5.1,5.1,4.1,2.1))
plot(time.30, Y.30, type="l", xlab="relative Zeit", ylab="relatives Wachstum",
     cex=2, lwd=3, cex.axis=2, cex.lab=2)
dev.off()



##############################################
# this part of the script plots all three confidence bands in one graphic. The underlying assumptions
# is the of an AR(1) model with unknown phi
# works only for degree=5, because gls doesn't like poly ...
# zuerst Y.10

Plot.estimation.methods(Y.10, 5, "man/0-Latex/graphics/Stammzellen-10kPa/10kPa-method.pdf")

# dann Y.30
Plot.estimation.methods(Y.30, 5, "man/0-Latex/graphics/Stammzellen-30kPa/30kPa-method.pdf")

#################################################ü
#

# zuerst Y.10
Plot.degrees(Y.10, 5, "man/0-Latex/graphics/Stammzellen-10kPa/10kPa-poly.pdf")

# dann Y.30
Plot.degrees(Y.30, 5, "man/0-Latex/graphics/Stammzellen-30kPa/30kPa-poly.pdf")

#################################################ü
#

# zuerst Y.10
Plot.poly.KB(Y.10, c(4,5,6), "man/0-Latex/graphics/Stammzellen-10kPa/10kPa-poly-KB")

# dann Y.30
Plot.poly.KB(Y.30, c(4,5,6), "man/0-Latex/graphics/Stammzellen-30kPa/30kPa-poly-KB")
