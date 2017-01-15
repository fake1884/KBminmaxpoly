# Testet, ob das wahre Modell ganz im Konfidenzband enthalten ist.
# Wiederlegt die Nullhypothese, falls die untere Grenze des KB über den wahren Daten oder die obere Grenze des
# KB unter den wahren Daten liegt.
# Auch diese Funktion unterscheidet die Modell R, AR-bekannt, AR und die Prüfvarianten

Test.function <- function(ngrid, untere.Grenze, obere.Grenze, model.type){

  #grind=matrix()

  band.richtig=T
  if(model.type == "R"){
    for(i in 1:ngrid)
    {
    # Das untere Konfidenzband ist über dem wahren Modell oder
    # Das obere Konfidenzband ist unter dem wahren Modell
    if(untere.Grenze[i] > data_R_true[i] | obere.Grenze[i] < data_R_true[i]){band.richtig=F}
    }
  }
  else if(model.type == "AR" | model.type == "AR-bekannt"){
    for(i in 1:ngrid)
    {
      # Das untere Konfidenzband ist über dem wahren Modell oder
      # Das obere Konfidenzband ist unter dem wahren Modell
      if(untere.Grenze[i] > data_AR_true[i] | obere.Grenze[i] < data_AR_true[i]){band.richtig=F}
    }
  }
  else if(model.type == "R-pruef"){

    for(i in 1:ngrid)
    {
      # Das untere Konfidenzband ist über dem wahren Modell oder
      # Das obere Konfidenzband ist unter dem wahren Modell
      if(untere.Grenze[i] > data_R_pruef_true[i] | obere.Grenze[i] < data_R_pruef_true[i]){band.richtig=F}
    }
  }
  else if(model.type == "AR-bekannt-pruef"){


    for(i in 1:ngrid)
    {
      # Das untere Konfidenzband ist über dem wahren Modell oder
      # Das obere Konfidenzband ist unter dem wahren Modell
      if(untere.Grenze[i] > data_AR_pruef_true[i] | obere.Grenze[i] < data_AR_pruef_true[i]){band.richtig=F}
    }
  }
  else if(model.type == "AR-pruef"){


    for(i in 1:ngrid)
    {
      # Das untere Konfidenzband ist über dem wahren Modell oder
      # Das obere Konfidenzband ist unter dem wahren Modell
      if(untere.Grenze[i] > data_AR_pruef_true[i] | obere.Grenze[i] < data_AR_pruef_true[i]){band.richtig=F}
    }
  }
  else(return("error"))

  return(band.richtig)
}

