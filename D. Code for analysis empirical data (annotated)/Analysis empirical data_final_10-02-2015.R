#Source required functions and load packages

source(file="C. Bayesian sampler/Regression sampler MH.R")
require(foreign)
require(xtable)
require(circular)
require(haven)

#Load restructured data without teachers with missing values
OWK_data<-read_spss(file="B. Empirical data & manipulation/empirical_data.csv")

data.matrix(OWK_data)

#Compute the circular Agency-Communion score and transform to degrees
Score_radians_start_c<-atan2(OWK_data$Agency.1, OWK_data$Communion.1)%%2*pi
Score_radians_sp<-atan2(OWK_data$Agency_sp.1, OWK_data$Communion_sp.1)%%2*pi
Score_degrees_start_c<-Score_radians_start_c*(180/pi)
Score_degrees_sp<-Score_radians_sp*(180/pi)

#Column bind to create a complete data file
OWK_DATA<- cbind(OWK_data, Score_radians_start_c, Score_radians_sp,
                 Score_degrees_sp, Score_degrees_start_c) 

#summary stats
summary(OWK_DATA)
sd(OWK_DATA$ex_c);sd(OWK_DATA$leserv_c)
summary(as.circular(OWK_DATA$Score_degrees_start_c, units = "degrees"))

#retrieve data for 4 teachers example (Table 1)
OWK_DATA[c(15,34,31,43),]

OWK_DATA_short<-as.data.frame(cbind(OWK_DATA$Score_radians_sp,
                                    OWK_DATA$Score_radians_start_c,
                                    OWK_DATA$leserv_c, DATA$ex_c))
colnames(OWK_DATA_short)<-c("Communion_Agency_sp","Communion_Agency_start",
                            "Teaching_experience","Extraversion")

#Create a new datafile (sampler requires names to be theta, x1, x2, etc.) for data 
theta<-OWK_DATA_short$Communion_Agency_start
x1<-cos(OWK_DATA_short$Communion_Agency_sp)
x2<-sin(OWK_DATA_short$Communion_Agency_sp)
x3<-OWK_DATA_short$Extraversion
x4<-OWK_DATA_short$Teaching_experience
summary(cbind(x1, x2))
sd(x1);sd(x2)
test<-as.data.frame(cbind(theta,x1,x2,x3,x4))

#Run the sampler and save the estimates
set.seed(101)
results<-CircReg(theta~x1+x2+x4, theta~x1+x2+x4+x3, data=test, tm=5000, t.lag=1, burn=750, flag.lk=FALSE)

#Obtain summary statistics for the posterior distributions of intercept and coefficients
summary(results[,1:9])

#Obtain histogram, autocorrelation and convergence plot for all coefficients and the intercept
names<-c("Intercept_1","Communion_1","Agency_1","Teaching_experience_1",
         "Intercept_2","Communion_2","Agency_2","Teaching_experience_2",
         "Extraversion_2")  
quantiles<-cbind(rep(NA,9), rep(NA,9))

for(i in 1:9){

pdf(paste("Specify your own pathname here", names[i], ".pdf", sep=""))
  par(mfrow=c(3,1))
  
hist(results[,i], main=paste(names[i]),
     xlab="Estimate", cex.lab=16/12, cex.axis=16/12)

abline(v=as.numeric(quantile(results[,i], probs=0.025)))

abline(v=as.numeric(quantile(results[,i], probs=0.975)))

acf(results[,i], main=paste(names[i]),
    cex.lab=16/12, cex.axis=16/12, cex.main=100/12)

plot.ts(results[,i], main=paste(names[i]),
        xlab="", ylab="", xaxt = 'n', yaxt = 'n')

axis(1, cex.axis = 16/12) 
axis(2, cex.axis = 16/12) 

title(xlab = "Iteration", ylab ="Estimate", cex.lab = 16/12)

dev.off()

quantiles[i,1]<-quantile(results[,i], probs=0.025)
quantiles[i,2]<-quantile(results[,i], probs=0.975)

}


X <- seq(-9.8, 18.70, 0.1)
eta <- ((mean(results[,1]) + mean(results[,4])*X)^2 +
        (mean(results[,5]) + mean(results[,8])*X)^2)/4
rbar <- sqrt(pi*0.5*eta)*exp(-eta)*(besselI(eta,0)+besselI(eta,1))

