Coverage.prob.pruef.fun <- function(model.type, est.method, niter, ngridpoly){

  if(model.type=="R-pruef"){

    if(est.method=="R"){
      #####################################################
      # Konfidenzband auf ganz R
      nerfolg = 100
      for(i in 1:support_data_R_pruef$ntest)
      {
        # kritischen Wert berechnen
        # alpha, nobs, k
        par.bsp.R=KB.R.pruef(support_data_R_pruef$alpha, length(data_R_pruef_test[,i]),
                             support_data_R_pruef$grad, support_data_R_pruef$k)

        # Konfidenzband bestimmen
        # grad, inv.X, beta, sigma, factor, k, ngrid
        plot.KB.R=plot.KB.pruef( nobs = support_data_R_pruef$nobs,
                                 grad = support_data_R_pruef$grad,
                                 inv.X = support_data_R_pruef$X.mat.inv,
                                 beta = data_modelR_pruef_estR_beta[,i],
                                 sigma = data_modelR_pruef_estR_sigma[i],
                                factor = par.bsp.R[[1]], k = support_data_R_pruef$k,
                                ngrid = support_data_R_pruef$ngrid)

        # liegt das wahre modell in dem Konfidenzband?
        if(Test.function(support_data_R_pruef$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
          nerfolg=nerfolg-1
        }
      }

      return(nerfolg)
    }
    else if(est.method=="minmax"){
      #################################################################
      #Schätz mit minmax

      nerfolg="fehlt noch"

      return(nerfolg)

    }
    else if(est.method=="minmax-poly"){
      #Schätz mit minmax-poly

      #########################################################
      nerfolg="fehlt noch"

      return(nerfolg)

    }
    else if(est.method=="minmax-poly-fast"){
      #schätz mit minmax-poly-fast

      ########################################################
      nerfolg = 100
      for(i in 1:support_data_R_pruef$ntest)
      {
      # kritischen Wert berechnen
      # alpha, nobs, grad, niter, inv.X, a, b, ngridpoly
      par.bsp.R=KB.poly.fast(support_data_R_pruef$alpha, length(data_R_pruef_test[,i]),
                             support_data_R_pruef$k-1,
                             niter, support_data_R_pruef$V,
                             support_data_R_pruef$a, support_data_R_pruef$b, ngridpoly)

      # Konfidenzband bestimmen
      plot.KB.R=plot.KB.pruef( nobs = support_data_R_pruef$nobs,
                               grad = support_data_R_pruef$grad,
                               inv.X = support_data_R_pruef$X.mat.inv,
                               beta = data_modelR_pruef_estR_beta[,i],
                               sigma = data_modelR_pruef_estR_sigma[i],
                               factor = par.bsp.R[[1]], k = support_data_R_pruef$k,
                               ngrid = support_data_R_pruef$ngrid)

      # liegt das wahre modell in dem Konfidenzband?
      if(Test.function(support_data_R_pruef$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
        nerfolg=nerfolg-1
         }
      }

      return(nerfolg)

    }
  }

  if(model.type=="AR-bekannt-pruef"){
    #estimate AR-bekannt model
    if(est.method=="R"){
      #Schätz mit R

      #######################################
      nerfolg=100
      for(i in 1:support_data_AR_pruef$ntest)
      {
        # kritischen Wert berechnen
        # alpha, nobs, k
        par.bsp.R=KB.R.pruef(support_data_AR_pruef$alpha, length(data_AR_pruef_test[,i]),
                             support_data_AR_pruef$grad, support_data_AR_pruef$k)

        # Konfidenzband bestimmen
        # nobs, grad, inv.X, beta, sigma, factor, k, ngrid
        plot.KB.R=plot.KB.pruef( nobs = support_data_AR_pruef$nobs,
                                 grad = support_data_AR_pruef$grad,
                                 inv.X = support_data_AR_pruef$X.mat.inv,
                                 beta = data_modelAR_pruef_estAR_bekannt_beta[,i],
                                 sigma = data_modelAR_pruef_estAR_bekannt_sigma[i],
                                 factor = par.bsp.R[[1]], k = support_data_AR_pruef$k,
                                 ngrid = support_data_AR_pruef$ngrid)

        # liegt das wahre modell in dem Konfidenzband?
        if(Test.function(support_data_AR_pruef$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
          nerfolg=nerfolg-1
        }
      }


      return(nerfolg)
    }
    else if(est.method=="minmax"){
      ###################################################
      #Schätz mit minmax

      nerfolg="fehlt noch"

      return(nerfolg)
    }
    else if(est.method=="minmax-poly"){
      #Schätz mit minmax-poly

      ####################################################
      nerfolg="fehlt noch"

      return(nerfolg)
    }
    else if(est.method=="minmax-poly-fast"){
      #schätz mit minmax-poly-fast

      ####################################################
      nerfolg = 100
      for(i in 1:support_data_AR_pruef$ntest)
      {
        # kritischen Wert berechnen
        # alpha, nobs, grad, niter, inv.X, a, b, ngridpoly
        par.bsp.R=KB.poly.fast(support_data_AR_pruef$alpha, length(data_AR_pruef_test[,i]),
                               support_data_AR_pruef$k-1,
                               niter, support_data_AR_pruef$V,
                               support_data_AR_pruef$a, support_data_AR_pruef$b, ngridpoly)

        # Konfidenzband bestimmen
        plot.KB.R=plot.KB.pruef( nobs = support_data_AR_pruef$nobs,
                                 grad = support_data_AR_pruef$grad,
                                 inv.X = support_data_AR_pruef$X.mat.inv,
                                 beta = data_modelAR_pruef_estAR_bekannt_beta[,i],
                                 sigma = data_modelAR_pruef_estAR_bekannt_sigma[i],
                                 factor = par.bsp.R[[1]], k = support_data_AR_pruef$k,
                                 ngrid = support_data_AR_pruef$ngrid)

        # liegt das wahre modell in dem Konfidenzband?
        if(Test.function(support_data_AR_pruef$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
          nerfolg=nerfolg-1
        }
      }

      return(nerfolg)
    }
  }
  else if(model.type=="AR-pruef"){
    #estimate AR model
    if(est.method=="R"){
      #Schätz mit R

      #################################################
      nerfolg=100
      for(i in 1:support_data_AR_pruef$ntest)
      {
        # kritischen Wert berechnen
        # alpha, nobs, k
        par.bsp.R=KB.R.pruef(support_data_AR_pruef$alpha, length(data_AR_pruef_test[,i]),
                             support_data_AR_pruef$grad, support_data_AR_pruef$k)

        # Konfidenzband bestimmen
        # nobs, grad, inv.X, beta, sigma, factor, k, ngrid
        plot.KB.R=plot.KB.pruef( nobs = support_data_AR_pruef$nobs,
                                 grad = support_data_AR_pruef$grad,
                                 inv.X = support_data_AR_pruef$X.mat.inv,
                                 beta = data_modelAR_pruef_estAR_beta[,i],
                                 sigma = data_modelAR_pruef_estAR_sigma[i],
                                 factor = par.bsp.R[[1]], k = support_data_AR_pruef$k,
                                 ngrid = support_data_AR_pruef$ngrid)

        # liegt das wahre modell in dem Konfidenzband?
        if(Test.function(support_data_AR_pruef$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
          nerfolg=nerfolg-1
        }
      }

      return(nerfolg)
    }
    else if(est.method=="minmax"){
      #Schätz mit minmax

      ####################################################
      nerfolg="fehlt noch"

      return(nerfolg)
    }
    else if(est.method=="minmax-poly"){
      #Schätz mit minmax-poly

      ########################################################
      nerfolg="fehlt noch"

      return(nerfolg)
    }
    else if(est.method=="minmax-poly-fast"){
      #schätz mit minmax-poly-fast

      ###########################################################
      nerfolg = 100
      for(i in 1:support_data_AR_pruef$ntest)
      {
        # kritischen Wert berechnen
        # alpha, nobs, grad, niter, inv.X, a, b, ngridpoly
        par.bsp.R=KB.poly.fast(support_data_AR_pruef$alpha, length(data_AR_pruef_test[,i]),
                               support_data_AR_pruef$k-1,
                               niter, support_data_AR_pruef$V,
                               support_data_AR_pruef$a, support_data_AR_pruef$b, ngridpoly)

        # Konfidenzband bestimmen
        plot.KB.R=plot.KB.pruef( nobs = support_data_AR_pruef$nobs,
                                 grad = support_data_AR_pruef$grad,
                                 inv.X = support_data_AR_pruef$X.mat.inv,
                                 beta = data_modelAR_pruef_estAR_beta[,i],
                                 sigma = data_modelAR_pruef_estAR_sigma[i],
                                 factor = par.bsp.R[[1]], k = support_data_AR_pruef$k,
                                 ngrid = support_data_AR_pruef$ngrid)

        # liegt das wahre modell in dem Konfidenzband?
        if(Test.function(support_data_AR_pruef$nobs, plot.KB.R[[2]], plot.KB.R[[3]], model.type)==F){
          nerfolg=nerfolg-1
        }
      }

      return(nerfolg)
    }
  }
  else{print("error")}
}
