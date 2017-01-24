# liest die Stammzelldaten aus /home/henning/.../inst/extdata/... ein und speichert sie in /data
# dadurch steht sie allen funktionen dieses Paketes zur Verfügung
# einrücken fehlt :(

Convert.stem.data = function(){
  # 10 kPa daten einlesen und in Y.10 speichern
  for(i in 1:51)
    assign( paste("data",i,sep="") ,read.csv(paste("/home/henning/Dokumente/R/07-Bachelorarbeit-Konfidenzbaender/KBminmaxpoly/inst/extdata/10kPa-data/Cell",i,".csv",sep=""))[1:145,])

  ar.names <- paste(rep("data",51),1:51,sep="")
AR <- data.frame(get(ar.names[1])[,2])
for (i in 2:51)
  AR <- cbind(AR, get(ar.names[i])[,2])
colnames(AR) <- c(1:51)
AR <- as.matrix(AR)

# daten skalieren
for (i in 1:51)
  AR[,i]=AR[,i]/AR[1,i]*1000

# probably better to build the mean?
Y.10.raw <- apply(AR,1,mean)

# Daten skalieren
Y.10=Y.10.raw/max(Y.10.raw)

devtools::use_data(Y.10, overwrite = T)


########################
# 30 kPa Daten einlesen
for(i in 1:53)
  assign( paste("data",i,sep="") ,read.csv(paste("/home/henning/Dokumente/R/07-Bachelorarbeit-Konfidenzbaender/KBminmaxpoly/inst/extdata/30kPa-Data/Data/Cell",i,".csv",sep=""))[1:145,])

ar.names <- paste(rep("data",53),1:53,sep="")
AR <- data.frame(get(ar.names[1])[,2])
for (i in 2:53)
  AR <- cbind(AR, get(ar.names[i])[,2])
colnames(AR) <- c(1:53)
AR <- as.matrix(AR)  #contains areas of 53 cells over 144 time points

# relative Zellgr??en anstatt absoluten
# skalierung so, dass die varianz des fehlers sigma eine vern?nftige gr??e (100<sigma<1) hat
for (i in 1:53)
  AR[,i]=AR[,i]/AR[1,i]*1000

# probably better to build the mean?
Y.30.raw <- apply(AR,1,mean)

# Daten skalieren
Y.30=Y.30.raw/max(Y.30.raw)

devtools::use_data(Y.30, overwrite = T)


}
