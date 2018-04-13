##############################################################################################################
##	Functions:	                                                                                            ##   
##     Simulate circular regression data (based on Projected Normal distribution of the outcome variable)   ##
##                                                                                                          ##
##     - simregPN		       1 linear predictor                                                               ## 						                                        
##     - simregPN_2X       2 linear predictors                                                              ##
##     - simregPN_CIRC     1 circular predictor                                                             ##
##     - simregPN_CIRC_X   1 circular and 1 linear predictor                                                ##
##     - simregPN_DIFF     different predictors for sine and cosine component                               ##
##																	                                                                        ##
##	Arguments:	                                                                                            ##
##      n			  Sample size											                                                            ##
##			B1#			Beta of IV in the regression equation for sine component of DV		                          ##
##			B2#			Beta of IV in the regression equation for cosine component of DV		                        ##
##			a1			Intercept in the regression equation for sine component of DV		                            ##
## 			a2			Intercept in the regression equation for cosine component of DV	                            ##
##			mu			Mean of the distribution of the linear IV (X~N(mu,sd))					                            ##
##			sd			Standard deviation of the distribution of the linearIV (X~N(mu,sd)), by 	                  ##
##						  varying the spread of the circular variable can be varied		                                ##
##      mu_c    Mean direction of the circular IV                                                           ##
##      k       Concentration parameter of the circular IV                                                  ##
##      dep     indicator for dependency between linear IVs (0=independent, 1=dependent)                    ##
##																		                                                                      ##
##	Result:	                                                                                                ##
##      y1			vector of length n containing sin components DV values			                                ##
##			y2			vector of length n containing cos components DV values			                                ##
##			x1			vector of length n containing IV values		                                                  ##			
##      X2      vector of length n containing IV values  				                                            ##
##      X3      vector of length n containing IV values                                                     ##				
##			R			  vector of length n containing the euclidean norm of the vector with 	                      ##
##						  endpoints (0,0) and (y1,y2)								                                                  ##
##			theta		circular DV; vector of length n computed by atan2(U1,U2)			                              ##
##												                                                                                  ##						
##############################################################################################################


simregPN<-function(n, B11, B12,a1, a2, mu, sd){
	
  require(MASS)	

  x1	  <-	rep(NA,n)
  y1	  <-	rep(NA,n)
  y2	  <-	rep(NA,n)
  theta <-  rep(NA,n)
  R	    <-	rep(NA,n)
  
  ##sample IV and DV values##

  x1 <-	rnorm(n,mu,sd)
  
    for(i in 1:n){
      
      y		  <-	mvrnorm(1,c((a1+(x1[i]*B11)),(a2+(x1[i]*B12))),cbind(c(1,0),c(0,1)))
      y1[i]	<-	y[1]
      y2[i]	<-	y[2]
      R[i]	<-	sqrt(y1[i]^2+y2[i]^2)
    
      ##compute circular DV##
    
      theta[i]  <-	atan2(y2[i],y1[i])%%(2*pi)
      
    }
  
  results<-cbind(x1,y1,y2,theta)
  
  return(results)

}


simregPN_2X<-function(n, B11, B12, B21, B22, a1, a2, mu, sd, dep){
	
  require(MASS)	

  x1	<-	rep(0,n)
  x2	<-	rep(0,n)
  y1	<-	rep(0,n)
  y2	<-	rep(0,n)
  R	  <-	rep(0,n)

    ##sample IV and DV values##


    for(i in 1:n){

     if(dep==0){
     x1[i]	<-	rnorm(1,mu[1],sd[1])
     x2[i]	<-	rnorm(1,mu[1],sd[1])
    }else{
     x		  <-	mvrnorm(1,c(0,0),cbind(c(1,dep),c(dep,1)))
     x1[i]	<-	x[1]
     x2[i]	<-	x[2]
    }

     y		  <-	mvrnorm(1,c((a1+(x1[i]*B11)+(x2[i]*B12)),(a2+(x1[i]*B21)+(x2[i]*B22))),cbind(c(1,0),c(0,1)))
     y1[i]	<-	y[1]
     y2[i]	<-	y[2]
     R[i]	  <-	sqrt(y1[i]^2+y2[i]^2)
    
    }


  ##compute circular DV##

  theta	<-	atan2(y2,y1)%%(2*pi)

  results<-cbind(x1,x2,y1,y2,theta)
  
  return(results)

}

simregPN_CIRC<-function(n, B11, B12, B21, B22, a1, a2, mu_c, k){
  
  require(CircStats)  
  require(MASS)	
  
  x1	  <-	rep(NA,n)
  x2    <-	rep(NA,n)
  y1	  <-	rep(NA,n)
  y2	  <-	rep(NA,n)
  theta <-  rep(NA,n)
  R	    <-	rep(NA,n)
  
  ##sample IV and DV values##
  
  x  <-	rvm(n,mu_c,k)
  x1 <- cos(x)-mean(cos(x))
  x2 <- sin(x)-mean(sin(x))
    
  for(i in 1:n){
    
    y  	  <-	mvrnorm(1,c((a1+(x1[i]*B11)+(x2[i]*B12)),(a2+(x1[i]*B21)+(x2[i]*B22))),cbind(c(1,0),c(0,1)))
    y1[i]	<-	y[1]
    y2[i]	<-	y[2]
    R[i]	<-	sqrt(y1[i]^2+y2[i]^2)
    
    ##compute circular DV##
    
    theta[i]  <-	atan2(y2[i],y1[i])%%(2*pi)
    
  }
  
  results<-cbind(x1,x2,y1,y2,theta)
  
  return(results)
  
}

simregPN_CIRC_nocent<-function(n, B11, B12, B21, B22, a1, a2, mu_c, k){
  
  require(CircStats)  
  require(MASS)	
  
  x1	  <-	rep(NA,n)
  x2    <-	rep(NA,n)
  y1	  <-	rep(NA,n)
  y2	  <-	rep(NA,n)
  theta <-  rep(NA,n)
  R	    <-	rep(NA,n)
  
  ##sample IV and DV values##
  
  x  <-	rvm(n,mu_c,k)
  x1 <- cos(x)
  x2 <- sin(x)
  
  for(i in 1:n){
    
    y  	  <-	mvrnorm(1,c((a1+(x1[i]*B11)+(x2[i]*B12)),(a2+(x1[i]*B21)+(x2[i]*B22))),cbind(c(1,0),c(0,1)))
    y1[i]	<-	y[1]
    y2[i]	<-	y[2]
    R[i]	<-	sqrt(y1[i]^2+y2[i]^2)
    
    ##compute circular DV##
    
    theta[i]  <-	atan2(y2[i],y1[i])%%(2*pi)
    
  }
  
  results<-cbind(x1,x2,y1,y2,theta)
  
  return(results)
  
}

simregPN_CIRC_X<-function(n, B11, B12, B13, B21, B22, B23, a1, a2, mu_c, k, mu,sd){
  
  require(CircStats)  
  require(MASS)  
  
  x     <-	rep(NA,n)
  x1	  <-	rep(NA,n)
  x2    <-	rep(NA,n)
  x3    <-  rep(NA,n)
  y1	  <-	rep(NA,n)
  y2	  <-	rep(NA,n)
  theta <-  rep(NA,n)
  R	    <-	rep(NA,n)
  
  ##sample IV and DV values##
  
  x  <-	rvm(n,mu_c,k)
  x1 <- cos(x)-mean(cos(x))
  x2 <- sin(x)-mean(sin(x))
  x3 <-  rnorm(n,mu,sd)
  
  for(i in 1:n){
    
    y  	  <-	mvrnorm(1,c((a1+(x1[i]*B11)+(x2[i]*B12)+(x3[i]*B13)),(a2+(x1[i]*B21)+(x2[i]*B22)+(x3[i]*B23))),cbind(c(1,0),c(0,1)))
    y1[i]	<-	y[1]
    y2[i]	<-	y[2]
    R[i]	<-	sqrt(y1[i]^2+y2[i]^2)
    
    ##compute circular DV##
    
    theta[i]  <-	atan2(y2[i],y1[i])%%(2*pi)
    
  }
  
  results<-cbind(x,x1,x2,x3,y1,y2,theta)
  
  return(results)
  
}

simregPN_DIFF<-function(n, B11, B12, B13, B21, B22, B23, B24, a1, a2, mu_c, k, mu1,sd1, mu2,sd2){
  
  require(CircStats)  
  require(MASS)  
  
  x     <-  rep(NA,n)
  x1	  <-	rep(NA,n)
  x2    <-	rep(NA,n)
  x3    <-  rep(NA,n)
  x4    <-  rep(NA,n)
  y1	  <-	rep(NA,n)
  y2	  <-	rep(NA,n)
  theta <-  rep(NA,n)
  R	    <-	rep(NA,n)
  
  ##sample IV and DV values##
  
  x  <-	rvm(n,mu_c,k)
  x1 <- cos(x)-mean(cos(x))
  x2 <- sin(x)-mean(sin(x))
  x3 <-  rnorm(n,mu1,sd1)
  x4 <-  rnorm(n,mu2,sd2)
  
  for(i in 1:n){
    
    y  	  <-	mvrnorm(1,c((a1+(x1[i]*B11)+(x2[i]*B12)+(x3[i]*B13)),(a2+(x1[i]*B21)+(x2[i]*B22)+(x3[i]*B23)+(x4[i]*B24))),cbind(c(1,0),c(0,1)))
    y1[i]	<-	y[1]
    y2[i]	<-	y[2]
    R[i]	<-	sqrt(y1[i]^2+y2[i]^2)
    
    ##compute circular DV##
    
    theta[i]  <-	atan2(y2[i],y1[i])%%(2*pi)
    
  }
  
  results<-cbind(x,x1,x2,x3,x4,y1,y2,theta)
  
  return(results)
  
}

