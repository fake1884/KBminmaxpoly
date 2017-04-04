# diese Funktion ist die Kernfunktion, um überdeckungswahrscheinlichkeiten zu bestimmen
# model.type in R, AR-bekannt, AR
# est.method in R, minmax, minmax-poly, minmax-poly-fast

Coverage.prob.fun = function(model.type, est.method, niter, ngridpoly){
  # zum schätzen der Parameter
  nerfolg=1000
  if(model.type=="R"){


    if(est.method=="R"){
      #####################################################
      # Konfidenzband auf ganz R

      # kritischen Wert berechnen
      par.bsp.R=KB.R(support_data_R$alpha, support_data_R$nobs, support_data_R$grad,
                     support_data_R$X.mat.inv)

      for(i in 1:support_data_R$ntest)
      {
        # Konfidenzband bestimmen
        plot.KB.R=plot.KB(support_data_R$nobs, support_data_R$grad, support_data_R$X.mat.inv,
                        data_modelR_estR_beta[,i], data_modelR_estR_sigma[i], par.bsp.R[[1]],
                        support_data_R$ngrid)

        # liegt das wahre modell in dem Konfidenzband?
        if(Test.function(support_data_R$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
          nerfolg=nerfolg-1
        }
      }
      return(nerfolg)
    }
    else if(est.method=="minmax"){
      #################################################################
      #Schätz mit minmax

      # kritischen Wert berechnen
      par.bsp.R=KB.minmax(support_data_R$alpha, support_data_R$nobs, support_data_R$grad , niter,
                            support_data_R$X.mat.inv, support_data_R$a, support_data_R$b)

      for(i in 1:support_data_R$ntest)
      {
        # Konfidenzband bestimmen
        plot.KB.R=plot.KB(support_data_R$nobs, support_data_R$grad, support_data_R$X.mat.inv,
                          data_modelR_estR_beta[,i], data_modelR_estR_sigma[i], par.bsp.R[[1]],
                          support_data_R$ngrid)

        # liegt das wahre modell in dem Konfidenzband?
        if(Test.function(support_data_R$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
          nerfolg=nerfolg-1
        }
      }
      return(nerfolg)

    }
    else if(est.method=="minmax-poly"){
      #Schätz mit minmax-poly

      # kritischen Wert berechnen
      par.bsp.R=KB.poly(support_data_R$alpha, support_data_R$nobs, support_data_R$grad,
                            niter,support_data_R$X.mat.inv, support_data_R$a,
                            support_data_R$b)

      #########################################################

      for(i in 1:support_data_R$ntest)
      {
        # Konfidenzband bestimmen
        plot.KB.R=plot.KB(support_data_R$nobs, support_data_R$grad, support_data_R$X.mat.inv,
                          data_modelR_estR_beta[,i], data_modelR_estR_sigma[i], par.bsp.R[[1]],
                          support_data_R$ngrid)

        # liegt das wahre modell in dem Konfidenzband?
        if(Test.function(support_data_R$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
          nerfolg=nerfolg-1
        }
      }

      return(nerfolg)

    }
    else if(est.method=="minmax-poly-fast"){
      #schätz mit minmax-poly-fast

      # kritischen Wert berechnen
      par.bsp.R=KB.poly.fast(support_data_R$alpha, support_data_R$nobs, support_data_R$grad,
                          niter,support_data_R$X.mat.inv, support_data_R$a,
                          support_data_R$b, ngridpoly)

      ########################################################
      for(i in 1:support_data_R$ntest)
      {
        # Konfidenzband bestimmen
        plot.KB.R=plot.KB(support_data_R$nobs, support_data_R$grad, support_data_R$X.mat.inv,
                          data_modelR_estR_beta[,i], data_modelR_estR_sigma[i], par.bsp.R[[1]],
                          support_data_R$ngrid)

        # liegt das wahre modell in dem Konfidenzband?
        if(Test.function(support_data_R$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
          nerfolg=nerfolg-1
        }
      }
      return(nerfolg)

    }
  }
  else if(model.type=="AR-bekannt"){
    #estimate AR-bekannt model
      if(est.method=="R"){
        #Schätz mit R

        # kritischen Wert berechnen
        par.bsp.R=KB.R(support_data_AR$alpha, support_data_AR$nobs, support_data_AR$grad,
                         support_data_AR$X.mat.inv)

        ######################################################################
        for(i in 1:support_data_AR$ntest)
        {
          # Konfidenzband bestimmen
          plot.KB.R=plot.KB(support_data_AR$nobs, support_data_AR$grad, support_data_AR$X.mat.inv,
                            data_modelAR_estAR_bekannt_beta[,i], data_modelAR_estAR_bekannt_sigma[i],
                            par.bsp.R[[1]], support_data_AR$ngrid)

          # liegt das wahre modell in dem Konfidenzband?
          if(Test.function(support_data_AR$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
            nerfolg=nerfolg-1
          }
        }
        return(nerfolg)
      }
      else if(est.method=="minmax"){
        #Schätz mit minmax

        # kritischen Wert berechnen
        par.bsp.AR=KB.minmax(support_data_AR$alpha, support_data_AR$nobs, support_data_AR$grad , niter,
                              support_data_AR$X.mat.inv, support_data_AR$a, support_data_AR$b)

        ###############################################################ü
        for(i in 1:support_data_R$ntest)
        {
          # Konfidenzband bestimmen
          plot.KB.R=plot.KB(support_data_AR$nobs, support_data_AR$grad, support_data_AR$X.mat.inv,
                            data_modelAR_estAR_bekannt_beta[,i], data_modelAR_estAR_bekannt_sigma[i],
                            par.bsp.R[[1]], support_data_AR$ngrid)

          # liegt das wahre modell in dem Konfidenzband?
          if(Test.function(support_data_AR$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
            nerfolg=nerfolg-1
          }
        }
        return(nerfolg)
      }
      else if(est.method=="minmax-poly"){
        #Schätz mit minmax-poly

        # kritischen Wert berechnen
        par.bsp.R=KB.poly(support_data_AR$alpha, support_data_AR$nobs, support_data_AR$grad,
                            niter,support_data_AR$X.mat.inv, support_data_AR$a,
                            support_data_AR$b)

        ##########################################################
        for(i in 1:support_data_AR$ntest)
        {
          # Konfidenzband bestimmen
          plot.KB.R=plot.KB(support_data_AR$nobs, support_data_AR$grad, support_data_AR$X.mat.inv,
                            data_modelAR_estAR_bekannt_beta[,i], data_modelAR_estAR_bekannt_sigma[i],
                            par.bsp.R[[1]], support_data_AR$ngrid)

          # liegt das wahre modell in dem Konfidenzband?
          if(Test.function(support_data_AR$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
            nerfolg=nerfolg-1
          }
        }
        return(nerfolg)
      }
      else if(est.method=="minmax-poly-fast"){
        #schätz mit minmax-poly-fast

        # kritischen Wert berechnen
        par.bsp.R=KB.poly.fast(support_data_AR$alpha, support_data_AR$nobs, support_data_AR$grad,
                                 niter,support_data_AR$X.mat.inv, support_data_AR$a,
                                 support_data_AR$b, ngridpoly)

        ##########################################################
        for(i in 1:support_data_AR$ntest)
        {
          # Konfidenzband bestimmen
          plot.KB.R=plot.KB(support_data_AR$nobs, support_data_AR$grad, support_data_AR$X.mat.inv,
                            data_modelAR_estAR_bekannt_beta[,i], data_modelAR_estAR_bekannt_sigma[i],
                            par.bsp.R[[1]], support_data_AR$ngrid)

          # liegt das wahre modell in dem Konfidenzband?
          if(Test.function(support_data_AR$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
            nerfolg=nerfolg-1
          }
        }
        return(nerfolg)

      }
  }
  else if(model.type=="AR"){
      #estimate AR model
      if(est.method=="R"){
        #Schätz mit R

        # kritischen Wert berechnen
        par.bsp.R=KB.R(support_data_AR$alpha, support_data_AR$nobs, support_data_AR$grad,
                         support_data_AR$X.mat.inv)

        ######################################################
        for(i in 1:support_data_AR$ntest)
        {
          # Konfidenzband bestimmen
          plot.KB.R=plot.KB(support_data_AR$nobs, support_data_AR$grad, support_data_AR$X.mat.inv,
                            data_modelAR_estAR_beta[,i], data_modelAR_estAR_sigma[i], par.bsp.R[[1]],
                            support_data_AR$ngrid) # alle sigmas in data_modelAR_estAR_sigma sind gleich :o
                                                   # deswegen wird nur ein sigma gespeichert oder auch nicht?!

          # liegt das wahre modell in dem Konfidenzband?
          if(Test.function(support_data_AR$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
            nerfolg=nerfolg-1
          }
        }
        return(nerfolg)
      }
      else if(est.method=="minmax"){
        #Schätz mit minmax

        # kritischen Wert berechnen
        par.bsp.R=KB.minmax(support_data_AR$alpha, support_data_AR$nobs, support_data_AR$grad , niter,
                              support_data_AR$X.mat.inv, support_data_AR$a, support_data_AR$b)

        ##########################################################
        for(i in 1:support_data_AR$ntest)
        {
          # Konfidenzband bestimmen
          plot.KB.R=plot.KB(support_data_AR$nobs, support_data_AR$grad, support_data_AR$X.mat.inv,
                            data_modelAR_estAR_beta[,i], data_modelAR_estAR_sigma[i], par.bsp.R[[1]],
                            support_data_AR$ngrid)

          # liegt das wahre modell in dem Konfidenzband?
          if(Test.function(support_data_AR$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
            nerfolg=nerfolg-1
          }
        }
        return(nerfolg)
      }
      else if(est.method=="minmax-poly"){
        #Schätz mit minmax-poly

        # kritischen Wert berechnen
        par.bsp.R=KB.poly(support_data_AR$alpha, support_data_AR$nobs, support_data_AR$grad,
                            niter, support_data_AR$X.mat.inv, support_data_AR$a,
                            support_data_AR$b)

        ##############################################################
        for(i in 1:support_data_AR$ntest)
        {
          # Konfidenzband bestimmen
          plot.KB.R=plot.KB(support_data_AR$nobs, support_data_AR$grad, support_data_AR$X.mat.inv,
                            data_modelAR_estAR_beta[,i], data_modelAR_estAR_sigma[i], par.bsp.R[[1]],
                            support_data_AR$ngrid)

          # liegt das wahre modell in dem Konfidenzband?
          if(Test.function(support_data_AR$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
            nerfolg=nerfolg-1
          }
        }
        return(nerfolg)

      }
      else if(est.method=="minmax-poly-fast"){
        #schätz mit minmax-poly-fast

        # kritischen Wert berechnen
        par.bsp.R=KB.poly.fast(support_data_AR$alpha, support_data_AR$nobs, support_data_AR$grad,
                                 niter,support_data_AR$X.mat.inv, support_data_AR$a,
                                 support_data_AR$b, ngridpoly)
        ################################################################ü
        for(i in 1:support_data_AR$ntest)
        {

          # Konfidenzband bestimmen
          plot.KB.R=plot.KB(support_data_AR$nobs, support_data_AR$grad, support_data_AR$X.mat.inv,
                            data_modelAR_estAR_beta[,i], data_modelAR_estAR_sigma[i], par.bsp.R[[1]],
                            support_data_AR$ngrid)

          # liegt das wahre modell in dem Konfidenzband?
          if(Test.function(support_data_AR$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
            nerfolg=nerfolg-1
          }
        }
        return(nerfolg)
      }
  }
  else{print("error")}
}
