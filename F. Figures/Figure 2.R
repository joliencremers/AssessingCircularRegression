#Load required packages
require(MASS)
require(plotrix)
library(extrafont)
font_install("fontcm")
loadfonts()

#Set seed
set.seed(101)

#create data
data<-mvrnorm(10, c(1.5,1.5), cbind(c(1,0),c(0,1)))
data2<-mvrnorm(10, c(3,3), cbind(c(1,0),c(0,1)))
data3<-mvrnorm(10, c(4.5,4.5), cbind(c(1,0),c(0,1)))

#Make plot
pdf("F. Figures/Figure2.pdf",
    width=10, height=5, family="CM Roman", pointsize=10)

postscript("F. Figures/Figure2.eps",
           horizontal = FALSE, onefile = FALSE, paper = "special",
           height=5, width=10, family="CM Roman", pointsize=10)

par(mfrow=c(1,3),
    oma=c(1,6,6,7),
    mar=c(5,6,0,0),
    las=1, mgp=c(4,1,0),
    xaxs="r", yaxs="r")

plot(x=data[,1], y=data[,2],
     xlim=c(-2,7), ylim=c(-2,7),
     ylab= "Y2", xlab="Y1",
     cex.lab=20/10, cex.axis=1.75)

abline(h=0)
abline(v=0)

draw.circle(0,0,1,nv=100,border=NULL,col=NA,lty=1,lwd=1)

for(i in 1:10){
  segments(0, 0, x1 = data[i,1], y1 = data[i,2], col="black")
}


plot(x=data2[,1], y=data2[,2],
     xlim=c(-2,7), ylim=c(-2,7),
     xlab="Y1", ylab="",
     yaxt='n',cex.lab=20/10, cex.axis=1.75)

abline(h=0)
abline(v=0)

draw.circle(0,0,1,nv=100,border=NULL,col=NA,lty=1,lwd=1)

for(i in 1:10){
  segments(0, 0, x1 = data2[i,1], y1 = data2[i,2], col="black")
}

plot(x=data3[,1], y=data3[,2],
     xlim=c(-2,7), ylim=c(-2,7),
     ylab="", xlab="Y1",
     yaxt='n',cex.lab=20/10, cex.axis=1.75)

abline(h=0)
abline(v=0)

draw.circle(0,0,1,nv=100,border=NULL,col=NA,lty=1,lwd=1)

for(i in 1:10){
  segments(0, 0, x1 = data3[i,1], y1 = data3[i,2], col="black")
}

dev.off()

