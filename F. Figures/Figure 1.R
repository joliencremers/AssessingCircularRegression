#Load required packages
require(calibrate)
require(plotrix)
require(extrafont)
font_install("fontcm")
loadfonts()

#Create a vector from the data for 4 teachers in radians
data<-cbind(cos(c(14.78*(pi/180),351.40*(pi/180),133.15*(pi/180),104.69*(pi/180))), 
            sin(c(14.78*(pi/180),351.40*(pi/180),133.15*(pi/180),104.69*(pi/180))))
#linear mean
mean(c(14.78, 351.40, 133.15, 104.69))
#circular mean
sin(sum(c(14.78, 351.40, 133.15, 104.69)))/cos(sum(c(14.78, 351.40, 133.15, 104.69)))

#Create plot
pdf("F. Figures/Figure1.pdf",
    family="CM Roman", pointsize=10)

postscript("F. Figures/Figure1.eps",
           horizontal = FALSE, onefile = FALSE,
           paper = "special", family="CM Roman",
           pointsize=10, height=5, width=5)

#Plot data and points for linear and circular mean
#Plot data from 4 teachers
plot(x=data[,1], y=data[,2],
     type="p", 
     xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2), 
     xlab="Agency", ylab="Communion", asp=1, 
     bty='n', xaxt ="n", yaxt='n', cex.lab=17/10)

#Plot linear mean
points(x=cos(c(151.01*(pi/180),0.5*pi)),
       y=sin(c(151.01*(pi/180),0)),
       type="l", lty=2)

#Plot circular mean
points(x=cos(c(0.5*pi,47.80*(pi/180))),
       y=sin(c(0,47.80*(pi/180))),
       type="l")

#Add horizontal and vertical line
abline(v=0, h=0)

#Draw a circle
draw.circle(0,0, radius = 1)

#Put in text
textxy(X=c(data[3,1],cos(151.01*(pi/180))), 
       Y=c(data[3,2],sin(151.01*(pi/180))), 
       labs=c("133.15","151.01"), 
       cex=1.5, offset=1, pos=c(2,2))
textxy(X=c(data[4,1]), 
       Y=c(data[4,2]), 
       labs=c("104.69"), 
       cex=1.5, offset=1, pos=c(3))
textxy(X=c(data[1:2,1],cos(47.8*(pi/180))), 
       Y=c(data[1:2,2],sin(47.8*(pi/180))), 
       labs=c("14.78","351.40","47.80"), 
       cex=1.5, offset=1, pos=c(4,4,4))

dev.off()

