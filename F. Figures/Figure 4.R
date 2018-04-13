#make vector with values from -2000 until 2000
A<-seq(-10, 20, 0.1)
require(plotrix)
library(extrafont)
font_install("fontcm")
loadfonts()

#create figures 5a,5b,5c,5d,5e,5f,5g 

####################################5a##########################################
pdf("F. Figures/figure5a.pdf", family="CM Roman")

#vary first predictor
par(mar=c(5,5,2,2))

Communion<-0.39+(0.54*A)-(0.13*0.34)-(0.02*0)
Agency<-1.67+(0.29*A)+(0.5*0.34)+(0.1*0)+(0.34*0)

#Separate predicted y for agency and communion
plot(y=Communion, x=A, type="l",
     ylim=c(-10,10), xlim=c(-1,1),
     ylab="Predicted Outcome", xlab="Communion Self-Perception",
     cex.lab=24/12, cex.axis=24/12)
points(y=Agency, x=A,type="l", lty=2)
dev.off()

####################################5e##########################################

pdf("F. Figures/figure5e.pdf", family="CM Roman")

#compute and plot circular Y
par(mar=c(5,5,2,2))
Circular<-atan2(Agency,Communion)%%(2*pi)

plot(y=Circular, x=A,
     type="l",
     ylim=c(0,2*pi), xlim=c(-1,1),
     ylab="Predicted Outcome", xlab="Communion Self-Perception",
     cex.lab=24/12, cex.axis=24/12)
dev.off()

####################################5b##########################################

pdf("F. Figures/figure5b.pdf", family="CM Roman")

#vary second predictor
par(mar=c(5,5,2,2))

Communion2<-0.39-(0.54*0.21)-(0.13*A)-(0.02*0)
Agency2<-1.67-(0.29*0.21)+(0.5*A)+(0.1*0)+(0.34*0)

#Separate predicted y for agency and communion
plot(y=Communion2, x=A,
     type="l",
     ylim=c(-10,10), xlim=c(-1,1),
     ylab="Predicted Outcome", xlab="Agency Self-Perception",
     cex.lab=24/12, cex.axis=24/12)
points(y=Agency2, x=A,type="l", lty=2)
dev.off()


####################################5f##########################################

pdf("F. Figures/figure5f.pdf", family="CM Roman")

#compute and plot circular Y
par(mar=c(5,5,2,2))
Circular2<-atan2(Agency2,Communion2)%%(2*pi)
plot(y=Circular2, x=A,
     type="l",
     ylim=c(0,2*pi), xlim=c(-1,1),
     ylab="Predicted Outcome", xlab="Agency Self-Perception",
     cex.lab=24/12, cex.axis=24/12)
dev.off()

####################################5c##########################################

pdf("F. Figures/figure5c.pdf", family="CM Roman")

#vary third predictor
par(mar=c(5,5,2,2))

Communion3<-0.39-(0.54*0.21)-(0.13*0.34)-(0.02*A)
Agency3<-1.67-(0.29*0.21)+(0.5*0.34)+(0.1*A)+(0.34*0)

#Separate predicted y for agency and communion
plot(y=Communion3, x=A,
     type="l",
     ylim=c(-10,10), xlim=c(-10,20),
     ylab="Predicted Outcome", xlab="Teacher Experience",
     cex.lab=24/12, cex.axis=24/12)
points(y=Agency3, x=A,type="l", lty=2)
dev.off()

####################################5g##########################################

pdf("F. Figures/figure5g.pdf", family="CM Roman")

#compute and plot circular Y
par(mar=c(5,5,2,2))
Circular3<-atan2(Agency3,Communion3)%%(2*pi)
plot(y=Circular3, x=A,
     type="l",
     ylim=c(0,2*pi), xlim=c(-10,20),
     ylab="Predicted Outcome", xlab="Teacher Experience",
     cex.lab=24/12, cex.axis=24/12)
dev.off()

####################################5d##########################################

pdf("F. Figures/figure5d.pdf", family="CM Roman")

#vary fourth predictor
par(mar=c(5,5,2,2))

Communion4<-0.39-(0.54*0.21)-(0.13*0.34)-(0.02*0)+0*A
Agency4<-1.67-(0.29*0.21)+(0.5*A)+(0.1*0)+(0.34*A)

#Separate predicted y for agency and communion
plot(y=Communion4, x=A, 
     type="l",
     ylim=c(-10,10), xlim=c(-3,2),
     ylab="Predicted Outcome", xlab="Extraversion",
     cex.lab=24/12, cex.axis=24/12)
points(y=Agency4, x=A,type="l", lty=2)
dev.off()

####################################5h##########################################

pdf("F. Figures/figure5h.pdf", family="CM Roman")

#compute and plot circular Y
par(mar=c(5,5,2,2))
Circular4<-atan2(Agency4,Communion4)%%(2*pi)
plot(y=c(Circular4[1:81]-(2*pi),Circular4[82]), x=A[1:82],
     type="l",
     ylim=c(-pi,pi), xlim=c(-3,2),
     ylab="Predicted Outcome", xlab="Extraversion",
     cex.lab=24/12, cex.axis=24/12)
points(y=Circular4[82:301], x=A[82:301], type="l")
dev.off()

