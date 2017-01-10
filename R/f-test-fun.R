# Funktionen die ich benutze als Datei
# Beschreibung nach dem Muster:
# Was die Funktion macht und Welche Eingabewerte benutzt werden
# Wie die Funktion funktioniert
# Welche Rückgabewerte erzeugt werden



###################################
# F-Test Funktion
# Diese Funktion führt einen F-Test durch.
# benutzt die Berechnung aus dem zweiten Kapitel
# Es wird der Wert der Teststatistik teststat und der kritische Wert q wie Quantil zur?ckgegeben

f.test <- function(alpha, grad.1, grad.2, data.1, data.2){

  # Berechnet die Werte die in der Teststatistik benutzt werden
  Werte.1=OLS(grad.1, data.1, length(data.1))
  inv.X.1=Werte.1[[1]]
  beta.1=Werte.1[[2]]
  sigma.1=Werte.1[[3]]

  Werte.2=OLS(grad.2, data.2, length(data.2))
  inv.X.2=Werte.2[[1]]
  beta.2=Werte.2[[2]]
  sigma.2=Werte.2[[3]]

  p.1=grad.1
  p.2=grad.2
  p=max(p.1,p.2)
  n.1=length(data.1)
  n.2=length(data.2)

  # betas  auf die richtige Länge bringen
  beta = Beta.calc(beta.1, beta.2)
  beta.1 = beta[[1]]
  beta.2 = beta[[2]]

  # deltas auf die richtige Länge bringen
  delta = Delta(inv.X.1, inv.X.2)
  inv.X.1 = delta[[1]]
  inv.X.2 = delta [[2]]

  # Berechnet die Teststatistik
  zaehler <- t(beta.1-beta.2) %*% (inv.X.1+inv.X.2) %*% (beta.1-beta.2) / (p+1)
  nenner <- ((n.1-p-1)/(n.1+n.2-2*p-2))*sigma.1 + (n.2-p-1)/(n.1+n.2-2*p-2)*sigma.2
  teststat <- zaehler/nenner

  # Berechnet den kritischen Wert
  # qf(p, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)
  # q <- qf(alpha,p+1,n.1+n.2-2*(p+1))
  q <- qf(alpha,p+1,(n.1+n.2)/2-p-1)

  # Bestimmt, ob die Teststatistik oder der kritische Wert gr??er ist
  # TRUE -> reject a.1 == 0 -> a.1 \neq 0
  estout = list(teststat,q)

  estout
}
