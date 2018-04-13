SumStat_l<-function(nsim,n,B11,B12,a1,a2,mu,sd,tm,burn,t.lag,seed=101,date,no, BURN=0, dataloc){
  
  #Load required packages 
  
  require(xtable)
  require(Rcpp)
  sourceCpp('H. Extra Functions/VenterMode.cpp')
  
  load(file=paste(dataloc, "results", no, ".Rdata", sep=""))
  
  #Create empty arrays
  simdata  <-  array(NA, dim=c(n, 2, nsim), dimnames=list(NULL, c("theta", "x1"), NULL))
  sumres	 <-	array(NA, dim=c((4),6,nsim), dimnames=list(c("B1a", "B1b", "B2a", "B2b"), c("Bias", "Coverage", "Interval width", "CoverageHPD", "aiwHPD", "relBias")))
  mean	   <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("Mean")))
  mode     <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("Mode")))
  median	 <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("Median")))
  SD		   <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("SD")))
  MSE		   <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("MSE")))
  lo95	   <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("lower bound 95% CI")))
  hi95	   <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("upper bound 95% CI")))
  lo95HPD     <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("lower bound 95% HPD")))
  hi95HPD	   <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("upper bound 95% HPD")))
  bias	   <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("Bias")))
  relbias <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("relBias")))
  coverage <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("Coverage")))
  coverageHPD <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("CoverageHPD")))
  aiw		   <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("aiw")))
  aiwHPD  	   <- array(NA, dim=c((4),1), dimnames=list(c("B1a", "B1b", "B2a", "B2b"),c("aiwHPD")))
  sum		   <-	array(NA, dim=c((4),8,nsim), dimnames=list(c("B1a", "B1b", "B2a", "B2b"), c("mean", "lo95", "hi95", "SD", "Median", "lo95HPD", "hi95HPD", "Mode"), NULL))
  
 
  
  for (i in 1:nsim){
    
      
    #Compute summary statistics for 1 dataset
    #Mean of B's and R
    
    sum[ ,1,i]	<-	apply(res[(BURN+1):tm,,i], 2, mean)
  
    #Credible Interval
    sum[ ,2,i]	<-	apply(res[(BURN+1):tm,,i], 2, quantile,.025) 
    sum[ ,3,i]	<-	apply(res[(BURN+1):tm,,i], 2, quantile,.975)
   
    #SE
    sum[ ,4,i]	<-	apply(res[(BURN+1):tm,,i], 2, sd)
    
    #Median
    sum[ ,5,i]	<-	apply(res[(BURN+1):tm,,i], 2, quantile,.5)
    
    #Credible Interval #2 & mode
    for(j in 1:4){
      sum[ j,6,i]  <-	hmodeci(res[(BURN+1):tm,j,i], 0.95)[1]
      sum[ j,7,i]	<-	hmodeci(res[(BURN+1):tm,j,i], 0.95)[2]
      sum[ j,8,i]  <-	hmode(res[(BURN+1):tm,j,i],0.1)
    }
    
    
    
    #Bias (computed as deviation of estimated posterior mean from the population value)
    
    sumres[1,1,i]	        <-	a1 -mean(res[(BURN+1):tm,1,i])
    sumres[2,1,i]	        <-	B11-mean(res[(BURN+1):tm,2,i])
    sumres[3,1,i]	        <-	a2 -mean(res[(BURN+1):tm,3,i])
    sumres[4,1,i]	        <-	B12-mean(res[(BURN+1):tm,4,i]) 
    
    
    #Are parameters in the Credible Interval (CI) for each simulation?
    
    sumres[1,2,i]	        <- (a1  > sum[1,2,i]  &  a1  < sum[1,3,i])
    sumres[2,2,i]	        <- (B11 > sum[2,2,i]  &  B11 < sum[2,3,i])
    sumres[3,2,i]	        <- (a2  > sum[3,2,i]  &  a2  < sum[3,3,i])
    sumres[4,2,i]	        <- (B12 > sum[4,2,i]  &  B12 < sum[4,3,i])
    
    sumres[1,4,i]          <- (a1  > sum[1,6,i]  &  a1  < sum[1,7,i])
    sumres[2,4,i]	        <- (B11 > sum[2,6,i]  &  B11 < sum[2,7,i])
    sumres[3,4,i]	        <- (a2  > sum[3,6,i]  &  a2  < sum[3,7,i])
    sumres[4,4,i]	        <- (B12 > sum[4,6,i]  &  B12 < sum[4,7,i])
    
    #Width of CI
    
    sumres[1,3,i]         <- sum[1,3,i]-sum[1,2,i]
    sumres[2,3,i]         <- sum[2,3,i]-sum[2,2,i]
    sumres[3,3,i]         <- sum[3,3,i]-sum[3,2,i]
    sumres[4,3,i]         <- sum[4,3,i]-sum[4,2,i]
    
    sumres[1,5,i]         <- sum[1,7,i]-sum[1,6,i]
    sumres[2,5,i]         <- sum[2,7,i]-sum[2,6,i]
    sumres[3,5,i]         <- sum[3,7,i]-sum[3,6,i]
    sumres[4,5,i]         <- sum[4,7,i]-sum[4,6,i]
    
    #Relative Bias
    
    sumres[1,6,i]	        <-	abs(a1)/abs(sumres[1,1,i])
    sumres[2,6,i]	        <-	abs(B11)/abs(sumres[2,1,i])
    sumres[3,6,i]	        <-	abs(a2)/abs(sumres[3,1,i])
    sumres[4,6,i]	        <-	abs(B12)/abs(sumres[4,1,i])
    
  }
  
    
  #Compute mean of summary statistics over all datasets
  SD[]	     <-    apply(cbind(sum[,4,]),1,mean)
  mean[] 	   <-    apply(cbind(sum[,1,]),1,mean)
  median[]	 <-    apply(cbind(sum[,5,]),1,mean)
  mode[]   <-    apply(cbind(sum[,8,]),1,mean)
  lo95[]	   <-    apply(cbind(sum[,2,]),1,mean)
  hi95[]	   <-    apply(cbind(sum[,3,]),1,mean)
  lo95HPD[]     <-    apply(cbind(sum[,6,]),1,mean)
  hi95HPD[]	   <-    apply(cbind(sum[,7,]),1,mean)
  bias[]	   <-    apply(cbind(sumres[,1,]),1,mean)
  relbias[] <-  abs(bias[]) / abs(c(a1, B11, a2, B12))
  MSE[]	     <-    bias[]^2+SD[]^2
  coverage[] <-    apply(cbind(sumres[,2,]),1,mean)*100
  coverageHPD[] <-    apply(cbind(sumres[,4,]),1,mean)*100
  aiw[]	     <-    apply(cbind(sumres[,3,]),1,mean)
  aiwHPD[]       <-    apply(cbind(sumres[,5,]),1,mean)
    
  #Create and empty array for a result summary table and give names to the columns
  parameters	    <-	array(0, dim=c(1,15), dimnames=list(c("metadata"), c("nsim", "n", "a1", "a2", "B11", "B12", "tm", "burn", "t.lag", "seed", "mu", "sd", "date", "no", "BURN")))
  parameters[1, ]	<-	c(nsim, n, a1, a2, B11, B12, tm, burn, t.lag, seed, mu, sd, date,no,BURN)
  
  #Column bind the means of summary statistics
  results		      <-	cbind(mean, median, mode, lo95, lo95HPD, hi95,hi95HPD, SD, MSE, bias, relbias, coverage, coverageHPD, aiw, aiwHPD)
  
  #Write the means of the summary statistics (and the metadata; input to simulation function) to a file
  write.table(results, paste(dataloc, "resultssimulationSUMMARY",date, "_#", no, ".txt", sep=""), sep="\t")
  write.table(parameters, paste(dataloc, "resultssimulationSUMMARY",date, "_#", no, ".txt", sep=""), sep="\t", append=T)
  
  #Create a simple latex table for the results
  print(xtable(results), type="latex", file=paste(dataloc, "resultssimulationLATEX",date, "_#", no, ".tex", sep=""))
  
  #Output of function are the means of the summary statistics and metadata (input in simulation function)
  list(results=results, parameters=parameters)
  
}

#2 linear predictors

SumStat_2l<-function(nsim,n,B11,B12,B21,B22,a1,a2,mu,sd,dep,tm,burn,t.lag, seed=101,date,no,BURN=0, dataloc){
  
  #Load required packages and set seed
  require(xtable)
  require(Rcpp)
  sourceCpp('H. Extra Functions/VenterMode.cpp')
    
  load(file=paste(dataloc, "results", no, ".Rdata", sep=""))
  
  #Create empty arrays
  simdata  <- array(NA, dim=c(n, 3, nsim), dimnames=list(NULL, c("theta", "x1", "x2"), NULL))
  sumres	 <-	array(NA, dim=c((6),6,nsim), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"), c("Bias", "Coverage", "Interval width", "CoverageHPD", "aiwHPD", "relBias")))
  mean	   <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("Mean")))
  median	 <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("Median")))
  mode   <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("Mode")))
  SD		   <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("SD")))
  MSE		   <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("MSE")))
  lo95	   <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("lower bound 95% CI")))
  hi95	   <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("upper bound 95% CI")))
  lo95HPD     <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("lower bound 95% HPD")))
  hi95HPD	   <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("upper bound 95% HPD")))
  bias	   <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("Bias")))
  relbias	   <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("relBias")))
  coverage <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("Coverage")))
  coverageHPD <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("CoverageHPD")))
  aiw		   <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("aiw")))
  aiwHPD  	   <- array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("aiwHPD")))
  sum		   <-	array(NA, dim=c((6),8,nsim), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"), c("mean", "lo95", "hi95", "SD", "Median", "lo95HPD", "hi95HPD", "Mode"), NULL))
  
  
  
  for (i in 1:nsim){
    
    #Compute summary statistics for 1 dataset
    #Mean of B's and R
    
    sum[ ,1,i]	<-	apply(res[(BURN+1):tm,,i], 2, mean)
    
    #Credible Interval
    sum[ ,2,i]	<-	apply(res[(BURN+1):tm,,i], 2, quantile,.025) 
    sum[ ,3,i]	<-	apply(res[(BURN+1):tm,,i], 2, quantile,.975)
    
    #SD
    sum[ ,4,i]	<-	apply(res[(BURN+1):tm,,i], 2, sd)
    
    #Median
    sum[ ,5,i]	<-	apply(res[(BURN+1):tm,,i], 2, quantile,.5)
    
    #Credible Interval #2 & mode
    for(j in 1:6){
      sum[ j,6,i]  <-  hmodeci(res[(BURN+1):tm,j,i], 0.95)[1]
      sum[ j,7,i]	<-	hmodeci(res[(BURN+1):tm,j,i], 0.95)[2]
      sum[ j,8,i]  <-	hmode(res[(BURN+1):tm,j,i],0.1)
    }
    
    #Bias (computed as deviation of estimated posterior mean from the population value)
    
    sumres[1,1,i]	        <-	a1 -mean(res[(BURN+1):tm,1,i])
    sumres[2,1,i]	        <-	B11-mean(res[(BURN+1):tm,2,i])
    sumres[3,1,i]         <-	B12-mean(res[(BURN+1):tm,3,i])
    sumres[4,1,i]	        <-	a2 -mean(res[(BURN+1):tm,4,i])
    sumres[5,1,i]	        <-	B21-mean(res[(BURN+1):tm,5,i]) 
    sumres[6,1,i]         <-	B22-mean(res[(BURN+1):tm,6,i])
    
    
    #Are parameters in the Credible Interval (CI) for each simulation?
    
    sumres[1,2,i]	        <- (a1  > sum[1,2,i]  &  a1  < sum[1,3,i])
    sumres[2,2,i]	        <- (B11 > sum[2,2,i]  &  B11 < sum[2,3,i])
    sumres[3,2,i]         <- (B12 > sum[3,2,i]  &  B12 < sum[3,3,i])
    sumres[4,2,i]	        <- (a2  > sum[4,2,i]  &  a2  < sum[4,3,i])
    sumres[5,2,i]	        <- (B21> sum[5,2,i]   &  B21 < sum[5,3,i])
    sumres[6,2,i]         <- (B22> sum[6,2,i]   &  B22 < sum[6,3,i])
    
    sumres[1,4,i]          <- (a1  > sum[1,6,i]  &  a1  < sum[1,7,i])
    sumres[2,4,i]	        <- (B11 > sum[2,6,i]  &  B11 < sum[2,7,i])
    sumres[3,4,i]         <- (B12 > sum[3,6,i]  &  B12 < sum[3,7,i])
    sumres[4,4,i]	        <- (a2  > sum[4,6,i]  &  a2  < sum[4,7,i])
    sumres[5,4,i]	        <- (B21> sum[5,6,i]   &  B21 < sum[5,7,i])
    sumres[6,4,i]         <- (B22> sum[6,6,i]   &  B22 < sum[6,7,i])
    
    #width of CI
    
    sumres[1,3,i]         <- sum[1,3,i]-sum[1,2,i]
    sumres[2,3,i]         <- sum[2,3,i]-sum[2,2,i]
    sumres[3,3,i]         <- sum[3,3,i]-sum[3,2,i]
    sumres[4,3,i]         <- sum[4,3,i]-sum[4,2,i]
    sumres[5,3,i]         <- sum[5,3,i]-sum[5,2,i]
    sumres[6,3,i]         <- sum[6,3,i]-sum[6,2,i]
    
    sumres[1,5,i]         <- sum[1,7,i]-sum[1,6,i]
    sumres[2,5,i]         <- sum[2,7,i]-sum[2,6,i]
    sumres[3,5,i]         <- sum[3,7,i]-sum[3,6,i]
    sumres[4,5,i]         <- sum[4,7,i]-sum[4,6,i]
    sumres[5,5,i]         <- sum[5,7,i]-sum[5,6,i]
    sumres[6,5,i]         <- sum[6,7,i]-sum[6,6,i]
        
    #Relative Bias
    
    sumres[1,6,i]	        <-	abs(a1)/abs(sumres[1,1,i])
    sumres[2,6,i]	        <-	abs(B11)/abs(sumres[2,1,i])
    sumres[3,6,i]	        <-	abs(B12)/abs(sumres[3,1,i])
    sumres[4,6,i]	        <-	abs(a2)/abs(sumres[4,1,i])
    sumres[5,6,i]	        <-	abs(B21)/abs(sumres[5,1,i])
    sumres[6,6,i]	        <-	abs(B22)/abs(sumres[6,1,i])
    
  }
  
    #Compute mean of summary statistics over all datasets
  SD[]	     <-    apply(cbind(sum[,4,]),1,mean)
  mean[] 	   <-    apply(cbind(sum[,1,]),1,mean)
  median[]	 <-    apply(cbind(sum[,5,]),1,mean)
  mode[]   <-    apply(cbind(sum[,8,]),1,mean)
  lo95[]	   <-    apply(cbind(sum[,2,]),1,mean)
  hi95[]	   <-    apply(cbind(sum[,3,]),1,mean)
  lo95HPD[]     <-    apply(cbind(sum[,6,]),1,mean)
  hi95HPD[]	   <-    apply(cbind(sum[,7,]),1,mean)
  bias[]	   <-    apply(cbind(sumres[,1,]),1,mean)
  relbias[] <-  abs(bias[]) / abs(c(a1, B11, B12, a2, B21, B22))
  MSE[]	     <-	   bias[]^2+SD[]^2
  coverage[] <-    apply(cbind(sumres[,2,]),1,mean)*100
  coverageHPD[] <-    apply(cbind(sumres[,4,]),1,mean)*100
  aiw[]	     <-    apply(cbind(sumres[,3,]),1,mean)
  aiwHPD[]       <-    apply(cbind(sumres[,5,]),1,mean)
   
  #Create and empty array for a result summary table and give names to the columns
  parameters	<-	array(0, dim=c(1,18), dimnames=list(c("metadata"), c("nsim", "n", "a1", "a2", "B11", "B12", "B21", "B22", " dep", "tm", "burn", "t.lag", "seed", "mu", "sd", "date", "no", "BURN")))
  parameters[1, ]	<-	c(nsim,n, a1,a2,B11,B12,B21,B22,dep,tm,burn,t.lag, seed, mu, sd, date,no,BURN)
  
  #Column bind the means of summary statistics
  results  	      <-	cbind(mean, median, mode, lo95, lo95HPD, hi95,hi95HPD, SD, MSE, bias, relbias, coverage, coverageHPD, aiw, aiwHPD)
  
  #Write the means of the summary statistics (and the metadata; input to simulation function) to a file
  write.table(results, paste(dataloc, "resultssimulationSUMMARY",date, "_#", no, ".txt", sep=""), sep="\t")
  write.table(parameters, paste(dataloc, "resultssimulationSUMMARY",date, "_#", no, ".txt", sep=""), sep="\t", append=T)
  
  #Create a simple latex table for the results
  print(xtable(results), type="latex", file=paste(dataloc, "resultssimulationLATEX",date, "_#", no, ".tex", sep=""))
  
  #Output of function are the means of the summary statistics and metadata (input in simulation function)
  list(results=results, parameters=parameters)
  
}

#1 circular predictor

SumStat_1c<-function(nsim,n,B11,B12,B21,B22,a1,a2,mu_c,k,tm,burn,t.lag, seed=101,date,no,BURN=0, dataloc){
  
  #Load required packages and set seed
  require(xtable)
  require(Rcpp)
  sourceCpp('H. Extra Functions/VenterMode.cpp')
  
  load(file=paste(dataloc, "results", no, ".Rdata", sep=""))
    
  #Create empty arrays
  simdata   <-  array(NA, dim=c(n, 3, nsim), dimnames=list(NULL, c("theta", "x1", "x2"), NULL))
  sumres	  <-	array(NA, dim=c((6),6,nsim), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"), c("Bias", "Coverage", "Interval width", "CoverageHPD", "aiwHPD", "relBias")))
  mean	    <-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("Mean")))
  median	  <-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("Median")))
  mode      <-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("Mode")))
  SD		    <-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("SD")))
  MSE		    <-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("MSE")))
  lo95	    <-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("lower bound 95% CI")))
  hi95     	<-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("upper bound 95% CI")))
  lo95HPD      <-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("lower bound 95% HPD")))
  hi95HPD     	<-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("upper bound 95% HPD")))
  bias	    <-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("Bias")))
  relbias	    <-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("relBias")))
  coverage	<-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("Coverage")))
  coverageHPD  <-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("CoverageHPD")))
  aiw		    <-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("aiw")))
  aiwHPD  	    <-  array(NA, dim=c((6),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"),c("aiwHPD")))
  sum		    <-	array(NA, dim=c((6),8,nsim), dimnames=list(c("B1a", "B1b1", "B1b2", "B2a", "B2b1", "B2b2"), c("mean", "lo95", "hi95", "SD", "Median", "lo95HPD", "hi95HPD", "Mode"), NULL))
  
  
  for (i in 1:nsim){
    
    #Compute summary statistics for 1 dataset
    #Mean of B's and R
    
    sum[ ,1,i]	<-	apply(res[(BURN+1):tm,,i], 2, mean)
    
    #Credible Interval
    sum[ ,2,i]	<-	apply(res[(BURN+1):tm,,i], 2, quantile,.025) 
    sum[ ,3,i]	<-	apply(res[(BURN+1):tm,,i], 2, quantile,.975)
    
    #SD
    sum[ ,4,i]	<-	apply(res[(BURN+1):tm,,i], 2, sd)
    
    #median
    sum[ ,5,i]	<-	apply(res[(BURN+1):tm,,i], 2, quantile,.5)
    
    #Credible Interval #2 & mode
    for(j in 1:6){
      sum[ j,6,i]  <-  hmodeci(res[(BURN+1):tm,j,i], 0.95)[1]
      sum[ j,7,i]  <-	hmodeci(res[(BURN+1):tm,j,i], 0.95)[2]
      sum[ j,8,i]  <-	hmode(res[(BURN+1):tm,j,i],0.1)
    }
    
    #Bias (computed as deviation of estimated posterior mean from the population value)
    
    sumres[1,1,i]	        <-	a1 -mean(res[(BURN+1):tm,1,i])
    sumres[2,1,i]	        <-	B11-mean(res[(BURN+1):tm,2,i])
    sumres[3,1,i]         <-	B12-mean(res[(BURN+1):tm,3,i])
    sumres[4,1,i]	        <-	a2 -mean(res[(BURN+1):tm,4,i])
    sumres[5,1,i]	        <-	B21-mean(res[(BURN+1):tm,5,i]) 
    sumres[6,1,i]         <-	B22-mean(res[(BURN+1):tm,6,i])
    
    
    #Are parameters in the Credible Interval (CI) for each simulation?
    
    sumres[1,2,i]	        <- (a1  > sum[1,2,i]  &  a1  < sum[1,3,i])
    sumres[2,2,i]	        <- (B11 > sum[2,2,i]  &  B11 < sum[2,3,i])
    sumres[3,2,i]         <- (B12 > sum[3,2,i]  &  B12 < sum[3,3,i])
    sumres[4,2,i]	        <- (a2  > sum[4,2,i]  &  a2  < sum[4,3,i])
    sumres[5,2,i]	        <- (B21> sum[5,2,i]   &  B21 < sum[5,3,i])
    sumres[6,2,i]         <- (B22> sum[6,2,i]   &  B22 < sum[6,3,i])
    
    sumres[1,4,i]          <- (a1  > sum[1,6,i]  &  a1  < sum[1,7,i])
    sumres[2,4,i]	        <- (B11 > sum[2,6,i]  &  B11 < sum[2,7,i])
    sumres[3,4,i]         <- (B12 > sum[3,6,i]  &  B12 < sum[3,7,i])
    sumres[4,4,i]	        <- (a2  > sum[4,6,i]  &  a2  < sum[4,7,i])
    sumres[5,4,i]	        <- (B21> sum[5,6,i]   &  B21 < sum[5,7,i])
    sumres[6,4,i]         <- (B22> sum[6,6,i]   &  B22 < sum[6,7,i])
    
    #Width of CI
    
    sumres[1,3,i]         <- sum[1,3,i]-sum[1,2,i]
    sumres[2,3,i]         <- sum[2,3,i]-sum[2,2,i]
    sumres[3,3,i]         <- sum[3,3,i]-sum[3,2,i]
    sumres[4,3,i]         <- sum[4,3,i]-sum[4,2,i]
    sumres[5,3,i]         <- sum[5,3,i]-sum[5,2,i]
    sumres[6,3,i]         <- sum[6,3,i]-sum[6,2,i]
    
    sumres[1,5,i]         <- sum[1,7,i]-sum[1,6,i]
    sumres[2,5,i]         <- sum[2,7,i]-sum[2,6,i]
    sumres[3,5,i]         <- sum[3,7,i]-sum[3,6,i]
    sumres[4,5,i]         <- sum[4,7,i]-sum[4,6,i]
    sumres[5,5,i]         <- sum[5,7,i]-sum[5,6,i]
    sumres[6,5,i]         <- sum[6,7,i]-sum[6,6,i]
    
    #Relative Bias
    
    sumres[1,6,i]	        <-	abs(a1)/abs(sumres[1,1,i])
    sumres[2,6,i]	        <-	abs(B11)/abs(sumres[2,1,i])
    sumres[3,6,i]	        <-	abs(B12)/abs(sumres[3,1,i])
    sumres[4,6,i]	        <-	abs(a2)/abs(sumres[4,1,i])
    sumres[5,6,i]	        <-	abs(B21)/abs(sumres[5,1,i])
    sumres[6,6,i]	        <-	abs(B22)/abs(sumres[6,1,i]) 
    
  }
  
  #Compute mean of summary statistics over all datasets
  SD[]	      <-  apply(cbind(sum[,4,]),1,mean)
  mean[] 	    <-  apply(cbind(sum[,1,]),1,mean)
  median[]	  <-  apply(cbind(sum[,5,]),1,mean)
  mode[]       <-  apply(cbind(sum[,8,]),1,mean)
  lo95[]	    <-  apply(cbind(sum[,2,]),1,mean)
  hi95[]	    <-  apply(cbind(sum[,3,]),1,mean)
  lo95HPD[]      <-  apply(cbind(sum[,6,]),1,mean)
  hi95HPD[]	    <-  apply(cbind(sum[,7,]),1,mean)
  bias[]	    <-  apply(cbind(sumres[,1,]),1,mean)
  relbias[]	    <-  abs(bias[]) / abs(c(a1, B11, B12, a2, B21, B22))
  MSE[]	      <-	bias[]^2+SD[]^2
  coverage[]  <-  apply(cbind(sumres[,2,]),1,mean)*100
  coverageHPD[]  <-  apply(cbind(sumres[,4,]),1,mean)*100
  aiw[]	      <-  apply(cbind(sumres[,3,]),1,mean)
  aiwHPD[]        <-  apply(cbind(sumres[,5,]),1,mean)
   
  #Create and empty array for a result summary table and give names to the columns
  parameters	    <-	array(0, dim=c(1,17), dimnames=list(c("metadata"), c("nsim", "n", "a1", "a2", "B11", "B12", "B21", "B22", "tm", "burn", "t.lag", "seed", "mu_c", "k", "date", "no", "BURN")))
  parameters[1, ]	<-	c(nsim,n, a1,a2,B11,B12,B21,B22,tm,burn,t.lag, seed, mu_c, k, date,no,BURN)
  
  #Column bind the means of summary statistics
  results          <-  cbind(mean, median, mode, lo95, lo95HPD, hi95,hi95HPD, SD, MSE, bias, relbias, coverage, coverageHPD, aiw, aiwHPD)
  
  #Write the means of the summary statistics (and the metadata; input to simulation function) to a file
  write.table(results, paste(dataloc, "resultssimulationSUMMARY",date, "_#", no, ".txt", sep=""), sep="\t")
  write.table(parameters, paste(dataloc, "resultssimulationSUMMARY",date, "_#", no, ".txt", sep=""), sep="\t", append=T)
  
  #Create a simple latex table for the results
  print(xtable(results), type="latex", file=paste(dataloc, "resultssimulationLATEX",date, "_#", no, ".tex", sep=""))
  
  #Output of function are the means of the summary statistics and metadata (input in simulation function)
  list(results=results, parameters=parameters)
  
}


#different equations for sine and cosine component

SumStat_diff<-function(nsim,n,B11,B12,B13,B21,B22,B23, B24,a1,a2,mu_c,k,mu1,sd1,mu2,sd2,tm,burn,t.lag, seed=101,date,no,BURN=0, dataloc){
  
  #Load required packages and set seed
  require(xtable)
  require(Rcpp)
  sourceCpp('H. Extra Functions/VenterMode.cpp')
  
  load(file=paste(dataloc, "results", no, ".Rdata", sep=""))
  
  #Create empty arrays
  simdata   <-    array(NA, dim=c(n, 5, nsim), dimnames=list(NULL, c("theta", "x1", "x2", "x3", "x4"), NULL))
  time      <-	  array(NA, dim=c(nsim))
  sumres	  <-	  array(NA, dim=c((9),6,nsim), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"), c("Bias", "Coverage", "Interval width", "CoverageHPD", "aiwHPD", "relBias")))
  mean	    <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("Mean")))
  mode      <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("Mode")))
  median	  <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("Median")))
  SD		    <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("SD")))
  MSE		    <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("MSE")))
  lo95	    <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("lower bound 95% CI")))
  hi95	    <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("upper bound 95% CI")))
  lo95HPD      <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("lower bound 95% HPD")))
  hi95HPD	    <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("upper bound 95% HPD")))
  bias	    <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("Bias")))
  relbias	    <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("relBias")))
  coverage	<-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("Coverage")))
  coverageHPD  <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("CoverageHPD")))
  aiw		    <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("aiw")))
  aiwHPD  	    <-    array(NA, dim=c((9),1), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"),c("aiwHPD")))
  sum		    <-	  array(NA, dim=c((9),8,nsim), dimnames=list(c("B1a", "B1b1", "B1b2", "B1b3", "B2a", "B2b1", "B2b2", "B2b3", "B2b4"), c("mean", "lo95", "hi95", "SD", "Median", "lo95HPD", "hi95HPD", "Mode"), NULL))
  
   
  for (i in 1:nsim){
    #Compute summary statistics for 1 dataset
    #Mean of B's and R
    
    sum[ ,1,i]	<-	apply(res[(BURN+1):tm,,i], 2, mean)
    
    #Credible Interval
    sum[ ,2,i]	<-	apply(res[(BURN+1):tm,,i], 2, quantile,.025) 
    sum[ ,3,i]	<-	apply(res[(BURN+1):tm,,i], 2, quantile,.975)
    
    #SE
    sum[ ,4,i]	<-	apply(res[(BURN+1):tm,,i], 2, sd)
    
    #Median
    sum[ ,5,i]	<-	apply(res[(BURN+1):tm,,i], 2, quantile,.5)
    
    #Credible Interval #2 & mode
    for(j in 1:9){
      sum[ j,6,i]  <-  hmodeci(res[(BURN+1):tm,j,i], 0.95)[1]
      sum[ j,7,i]  <-  hmodeci(res[(BURN+1):tm,j,i], 0.95)[2]
      sum[ j,8,i]  <-	hmode(res[(BURN+1):tm,j,i],0.1)
    }
    
    #Bias (computed as deviation of estimated posterior mean from the population value)
    
    sumres[1,1,i]	        <-	a1 -mean(res[(BURN+1):tm,1,i])
    sumres[2,1,i]	        <-	B11-mean(res[(BURN+1):tm,2,i])
    sumres[3,1,i]         <-	B12-mean(res[(BURN+1):tm,3,i])
    sumres[4,1,i]	        <-	B13 -mean(res[(BURN+1):tm,4,i])
    sumres[5,1,i]	        <-	a2-mean(res[(BURN+1):tm,5,i]) 
    sumres[6,1,i]         <-	B21-mean(res[(BURN+1):tm,6,i])
    sumres[7,1,i]         <- B22-mean(res[(BURN+1):tm,7,i])
    sumres[8,1,i]         <- B23-mean(res[(BURN+1):tm,8,i])
    sumres[9,1,i]         <- B24-mean(res[(BURN+1):tm,9,i])
    
    
    #Are parameters in the Credible Interval (CI) for each simulation?
    
    sumres[1,2,i]	        <- (a1  > sum[1,2,i]  &  a1  < sum[1,3,i])
    sumres[2,2,i]	        <- (B11 > sum[2,2,i]  &  B11 < sum[2,3,i])
    sumres[3,2,i]         <- (B12 > sum[3,2,i]  &  B12 < sum[3,3,i])
    sumres[4,2,i]         <- (B13 > sum[4,2,i]  &  B13 < sum[4,3,i])
    sumres[5,2,i]	        <- (a2  > sum[5,2,i]  &  a2  < sum[5,3,i])
    sumres[6,2,i]	        <- (B21> sum[6,2,i]   &  B21 < sum[6,3,i])
    sumres[7,2,i]         <- (B22> sum[7,2,i]   &  B22 < sum[7,3,i])
    sumres[8,2,i]         <- (B23> sum[8,2,i]   &  B23 < sum[8,3,i])
    sumres[9,2,i]         <- (B24> sum[9,2,i]   &  B24 < sum[9,3,i])
    
    sumres[1,4,i]          <- (a1  > sum[1,6,i]  &  a1  < sum[1,7,i])
    sumres[2,4,i]	        <- (B11 > sum[2,6,i]  &  B11 < sum[2,7,i])
    sumres[3,4,i]         <- (B12 > sum[3,6,i]  &  B12 < sum[3,7,i])
    sumres[4,4,i]         <- (B13 > sum[4,6,i]  &  B13 < sum[4,7,i])
    sumres[5,4,i]	        <- (a2  > sum[5,6,i]  &  a2  < sum[5,7,i])
    sumres[6,4,i]	        <- (B21> sum[6,6,i]   &  B21 < sum[6,7,i])
    sumres[7,4,i]         <- (B22> sum[7,6,i]   &  B22 < sum[7,7,i])
    sumres[8,4,i]         <- (B23> sum[8,6,i]   &  B23 < sum[8,7,i])
    sumres[9,4,i]         <- (B24> sum[9,6,i]   &  B24 < sum[9,7,i])
    
    #Width of CI
    
    sumres[1,3,i]         <- sum[1,3,i]-sum[1,2,i]
    sumres[2,3,i]         <- sum[2,3,i]-sum[2,2,i]
    sumres[3,3,i]         <- sum[3,3,i]-sum[3,2,i]
    sumres[4,3,i]         <- sum[4,3,i]-sum[4,2,i]
    sumres[5,3,i]         <- sum[5,3,i]-sum[5,2,i]
    sumres[6,3,i]         <- sum[6,3,i]-sum[6,2,i]
    sumres[7,3,i]         <- sum[7,3,i]-sum[7,2,i]
    sumres[8,3,i]         <- sum[8,3,i]-sum[8,2,i]
    sumres[9,3,i]         <- sum[9,3,i]-sum[9,2,i]
    
    sumres[1,5,i]         <- sum[1,7,i]-sum[1,6,i]
    sumres[2,5,i]         <- sum[2,7,i]-sum[2,6,i]
    sumres[3,5,i]         <- sum[3,7,i]-sum[3,6,i]
    sumres[4,5,i]         <- sum[4,7,i]-sum[4,6,i]
    sumres[5,5,i]         <- sum[5,7,i]-sum[5,6,i]
    sumres[6,5,i]         <- sum[6,7,i]-sum[6,6,i]
    sumres[7,5,i]         <- sum[7,7,i]-sum[7,6,i]
    sumres[8,5,i]         <- sum[8,7,i]-sum[8,6,i]
    sumres[9,5,i]         <- sum[9,7,i]-sum[9,6,i]
    
    #Relative Bias
    
    sumres[1,6,i]	        <-	abs(a1)/abs(sumres[1,1,i])
    sumres[2,6,i]	        <-	abs(B11)/abs(sumres[2,1,i])
    sumres[3,6,i]	        <-	abs(B12)/abs(sumres[3,1,i])
    sumres[4,6,i]	        <-	abs(B13)/abs(sumres[4,1,i])
    sumres[5,6,i]	        <-	abs(a2)/abs(sumres[5,1,i])
    sumres[6,6,i]	        <-	abs(B21)/abs(sumres[6,1,i])
    sumres[7,6,i]	        <-	abs(B22)/abs(sumres[7,1,i])
    sumres[8,6,i]	        <-	abs(B23)/abs(sumres[8,1,i])
    sumres[9,6,i]	        <-	abs(B24)/abs(sumres[9,1,i])
    
}
  
  #Compute mean of summary statistics over all datasets
  SD[]        <-  apply(cbind(sum[,4,]),1,mean)
  mean[] 	    <-  apply(cbind(sum[,1,]),1,mean)
  median[]	  <-  apply(cbind(sum[,5,]),1,mean)
  mode[]       <-  apply(cbind(sum[,8,]),1,mean)
  lo95[]	    <-  apply(cbind(sum[,2,]),1,mean)
  hi95[]	    <-  apply(cbind(sum[,3,]),1,mean)
  lo95HPD[]      <-  apply(cbind(sum[,6,]),1,mean)
  hi95HPD[]	    <-  apply(cbind(sum[,7,]),1,mean)
  bias[]	    <-  apply(cbind(sumres[,1,]),1,mean)
  relbias[]	    <-  abs(bias[]) / abs(c(a1, B11, B12, B13, a2, B21, B22, B23))
  MSE[]	      <-	bias[]^2+SD[]^2
  coverage[]  <-  apply(cbind(sumres[,2,]),1,mean)*100
  coverageHPD[]  <-  apply(cbind(sumres[,4,]),1,mean)*100
  aiw[]	      <-  apply(cbind(sumres[,3,]),1,mean)
  aiwHPD[]        <-  apply(cbind(sumres[,5,]),1,mean)
  MeanCompT		<- 	mean(time)
  
  #Create and empty array for a result summary table and give names to the columns
  parameters	    <-	array(0, dim=c(1,24), dimnames=list(c("metadata"), c("nsim", "n", "a1", "a2", "B11", "B12", "B13", "B21", "B22", "B23","B24", "tm", "burn", "t.lag", "seed", "mu_c", "k", "mu1","sd1", "mu2","sd2", "date", "no", "BURN")))
  parameters[1, ]	<-	c(nsim,n, a1,a2,B11,B12,B13,B21,B22,B23,B24,tm,burn,t.lag, seed, mu_c, k, mu1,sd1,mu2,sd2,date,no,BURN)
  
  #Column bind the means of summary statistics
  results          <-	cbind(mean, median, mode, lo95, lo95HPD, hi95,hi95HPD, SD, MSE, bias, relbias, coverage, coverageHPD, aiw, aiwHPD, MeanCompT)
  
  #Write the means of the summary statistics (and the metadata; input to simulation function) to a file
  write.table(results, paste(dataloc, "resultssimulationSUMMARY", date, "_#", no, ".txt", sep=""), sep="\t")
  write.table(parameters, paste(dataloc, "resultssimulationSUMMARY", date, "_#", no, ".txt", sep=""), sep="\t", append=T)

  #Create a simple latex table for the results
  print(xtable(results), type="latex", file=paste(dataloc, "resultssimulationLATEX",date, "_#", no, ".tex", sep=""))

  #Output of function are the means of the summary statistics and metadata (input in simulation function)
  list(results=results, parameters=parameters)
  
}
