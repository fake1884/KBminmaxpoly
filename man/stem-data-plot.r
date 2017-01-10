# Erzeugt die nackten Plots der Daten für die Arbeit

pdf("man/0-Latex/graphics/Stammzellen-10kPa/10kPa-data.pdf")
plot(time, Y.10, type="l", main="10 kPa", xlab="relative Zeit", ylab="relatives Wachstum")
dev.off()

pdf("man/0-Latex/graphics/Stammzellen-30kPa/30kPa-data.pdf")
plot(time, Y.30, type="l", main="30 kPa", xlab="relative Zeit", ylab="relatives Wachstum")
dev.off()



##############################################
# this part of the script plots all three confidence bands in one graphic. The underlying assumptions
# is the of an AR(1) model with unknown phi
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
Plot.poly.KB(Y.10, c(4,5,6), "man/0-Latex/graphics/Stammzellen-10kPa/10kPa-poly-KB.pdf")

# dann Y.30
Plot.poly.KB(Y.30, c(4,5,6), "man/0-Latex/graphics/Stammzellen-30kPa/30kPa-poly-KB.pdf")
