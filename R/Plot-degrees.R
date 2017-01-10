# Diese Funktion plottet Polynomregressions zu den Stammzelldaten mit verschiedenem Grad

Plot.degrees = function(data.set, degree, graphicspath){
  # Parameter, beta und sigma, bestimmen
  nobs = length(data.set)
  time=0:(nobs-1)/(nobs-1)
  time.2=time^2
  time.3=time^3
  time.4=time^4
  time.5=time^5
  time.6=time^6

  Y.gls.4 <- gls(data.set ~ time+I(time^2)+I(time^3)+I(time^4), correlation=corAR1())
  Y.gls.5 <- gls(data.set ~ time+I(time^2)+I(time^3)+I(time^4)+I(time^5), correlation=corAR1())
  Y.gls.6 <- gls(data.set ~ time+I(time^2)+I(time^3)+I(time^4)+I(time^5)+I(time^6), correlation=corAR1())



  ######################################
  # Graphik erzeugen
  pdf(graphicspath, width = 10, height = 8)
  time=1:144/144
  plot(time, data.set, type="l", xlab="relative Zeit", ylab="relatives Wachstum")
  curve(Y.gls.4$coefficients[1]+Y.gls.4$coefficients[2]*x+Y.gls.4$coefficients[3]*x^2+
          Y.gls.4$coefficients[4]*x^3+Y.gls.4$coefficients[5]*x^4, add=T, lwd=2, lty="solid")
  curve(Y.gls.5$coefficients[1]+Y.gls.5$coefficients[2]*x+Y.gls.5$coefficients[3]*x^2+
        Y.gls.5$coefficients[4]*x^3+Y.gls.5$coefficients[5]*x^4+Y.gls.5$coefficients[6]*x^5,
        add=T, lwd=2, lty="dotted")
  curve(Y.gls.6$coefficients[1]+Y.gls.6$coefficients[2]*x+Y.gls.6$coefficients[3]*x^2+
          Y.gls.6$coefficients[4]*x^3+Y.gls.6$coefficients[5]*x^4+Y.gls.6$coefficients[6]*x^5+
          Y.gls.6$coefficients[7]*x^6, add=T,
        lwd=2, lty="dashed")
  legend(x="topleft", legend=c("Grad 4", "Grad 5", "Grad 6"),
         col=c("red", "green", "blue"),cex=0.5, lty=c("solid", "dotted", "dashed"))
  dev.off()
}

